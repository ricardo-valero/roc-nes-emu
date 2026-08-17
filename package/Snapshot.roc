import /Nes
import /Cpu
import /Cpu/Register
import /Bus
import /Ppu
import /Apu
import /Cartridge

# Save states: the whole console as bytes and back.
#
# Layout (all little-endian): "RNES" magic, u16 format version (4: span
# rendering progress joined the PPU timing and MMC3 A12 state), u64 FNV-1a
# ROM hash, then every piece of state that affects future emulation - CPU,
# console bookkeeping, bus (internal RAM, PRG RAM, controller), PPU
# (registers, OAM, VRAM, palette, timing, framebuffer), APU (all five
# channels, frame counter), mapper registers, and CHR RAM when the
# cartridge uses it. Immutable ROM data (PRG/CHR ROM) is NOT included:
# decode grafts the snapshot onto a freshly loaded Cartridge and rejects a
# hash mismatch. A layout change bumps the version; old states are simply
# rejected (no migration).
#
# The APU's drained-sample buffer is transient frontend output, not console
# state, so it is not encoded - drain samples before snapshotting when
# comparing post-restore audio.
#
# Battery saves ride the same module: the 8 KiB PRG RAM slice raw, version-
# free, for cartridges whose header sets the battery flag.

# decode cursor
Cur : { bytes : List(U8), pos : U64 }

Snapshot :: [].{
    # --- byte writers (little-endian) ---

    bb : Bool -> U8
    bb = |b| if b { 1 } else { 0 }

    w16 : List(U8), U16 -> List(U8)
    w16 = |acc, v| acc.append(v.to_u8_wrap()).append(v.shr_zf_wrap(8).to_u8_wrap())

    w64 : List(U8), U64 -> List(U8)
    w64 = |acc0, v0| {
        go = |acc, v, k|
            if k >= 8 {
                acc
            } else {
                go(acc.append(v.to_u8_wrap()), v.shr_zf_wrap(8), k.plus(1))
            }
        k0 : U64
        k0 = 0
        go(acc0, v0, k0)
    }

    wi64 : List(U8), I64 -> List(U8)
    wi64 = |acc, v| w64(acc, v.to_u64_wrap())

    # --- byte readers ---
    # (left unannotated: a closed [Truncated] union would not widen through
    # `?` into decode's larger error set)

    r8 = |cur|
        match cur.bytes.get(cur.pos) {
            Ok(v) => Ok({ cur: { ..cur, pos: cur.pos.plus(1) }, v: v })
            Err(_) => Err(Truncated)
        }

    rb = |cur| {
        r = r8(cur)?
        Ok({ cur: r.cur, v: r.v != 0 })
    }

    rbytes = |cur, n|
        if cur.pos.plus(n) > cur.bytes.len() {
            Err(Truncated)
        } else {
            Ok({ cur: { ..cur, pos: cur.pos.plus(n) }, v: List.sublist(cur.bytes, { start: cur.pos, len: n }) })
        }

    r16 = |cur| {
        lo = r8(cur)?
        hi = r8(lo.cur)?
        Ok({ cur: hi.cur, v: hi.v.to_u16().shl_wrap(8).bitwise_or(lo.v.to_u16()) })
    }

    r64 = |cur| {
        r = rbytes(cur, 8)?
        folded = r.v.fold({ v: 0.U64, sh: 0.U8 }, |st, b| {
            { v: st.v.bitwise_or(b.to_u64().shl_wrap(st.sh)), sh: st.sh.plus(8) }
        })
        Ok({ cur: r.cur, v: folded.v })
    }

    ri64 = |cur| {
        r = r64(cur)?
        Ok({ cur: r.cur, v: r.v.to_i64_wrap() })
    }

    # --- controller buttons <-> one packed byte (hardware latch order) ---

    pack_buttons = |b| {
        bit = |on, n| if on { U8.shl_wrap(1, n) } else { 0 }
        bit(b.a, 0)
            .bitwise_or(bit(b.b, 1))
            .bitwise_or(bit(b.select, 2))
            .bitwise_or(bit(b.start, 3))
            .bitwise_or(bit(b.up, 4))
            .bitwise_or(bit(b.down, 5))
            .bitwise_or(bit(b.left, 6))
            .bitwise_or(bit(b.right, 7))
    }

    unpack_buttons = |v| {
        bit = |n| v.bitwise_and(U8.shl_wrap(1, n)) != 0.U8
        { a: bit(0), b: bit(1), select: bit(2), start: bit(3), up: bit(4), down: bit(5), left: bit(6), right: bit(7) }
    }

    mir_tag : [Horizontal, Vertical, FourScreen, SingleLow, SingleHigh] -> U8
    mir_tag = |m|
        match m {
            Horizontal => 0
            Vertical => 1
            FourScreen => 2
            SingleLow => 3
            SingleHigh => 4
        }

    # --- APU sub-encoders / decoders (field order is the format) ---

    w_env = |acc, e|
        acc.append(bb(e.start)).append(e.divider).append(e.decay).append(e.param).append(bb(e.const_vol))

    r_env = |cur| {
        s = rb(cur)?
        d = r8(s.cur)?
        de = r8(d.cur)?
        pa = r8(de.cur)?
        cv = rb(pa.cur)?
        Ok({ cur: cv.cur, v: { start: s.v, divider: d.v, decay: de.v, param: pa.v, const_vol: cv.v } })
    }

    w_pulse = |acc, p| {
        a1 = acc.append(bb(p.enabled)).append(p.duty).append(bb(p.halt))
        a2 = w_env(a1, p.env)
        a3 = a2.append(bb(p.sweep_on)).append(p.sweep_period).append(bb(p.sweep_negate)).append(p.sweep_shift).append(p.sweep_div).append(bb(p.sweep_reload)).append(bb(p.ones))
        w16(w16(a3, p.period), p.timer).append(p.seq).append(p.length)
    }

    r_pulse = |cur| {
        en = rb(cur)?
        du = r8(en.cur)?
        ha = rb(du.cur)?
        ev = r_env(ha.cur)?
        so = rb(ev.cur)?
        sp = r8(so.cur)?
        sn = rb(sp.cur)?
        ss = r8(sn.cur)?
        sd = r8(ss.cur)?
        sr = rb(sd.cur)?
        on = rb(sr.cur)?
        pe = r16(on.cur)?
        ti = r16(pe.cur)?
        sq = r8(ti.cur)?
        le = r8(sq.cur)?
        Ok({ cur: le.cur, v: {
            enabled: en.v,
            duty: du.v,
            halt: ha.v,
            env: ev.v,
            sweep_on: so.v,
            sweep_period: sp.v,
            sweep_negate: sn.v,
            sweep_shift: ss.v,
            sweep_div: sd.v,
            sweep_reload: sr.v,
            ones: on.v,
            period: pe.v,
            timer: ti.v,
            seq: sq.v,
            length: le.v,
        } })
    }

    w_triangle = |acc, t| {
        a1 = acc.append(bb(t.enabled)).append(bb(t.control)).append(t.reload_value).append(t.linear).append(bb(t.reload_flag))
        w16(w16(a1, t.period), t.timer).append(t.seq).append(t.length)
    }

    r_triangle = |cur| {
        en = rb(cur)?
        co = rb(en.cur)?
        rv = r8(co.cur)?
        li = r8(rv.cur)?
        rf = rb(li.cur)?
        pe = r16(rf.cur)?
        ti = r16(pe.cur)?
        sq = r8(ti.cur)?
        le = r8(sq.cur)?
        Ok({ cur: le.cur, v: {
            enabled: en.v,
            control: co.v,
            reload_value: rv.v,
            linear: li.v,
            reload_flag: rf.v,
            period: pe.v,
            timer: ti.v,
            seq: sq.v,
            length: le.v,
        } })
    }

    w_noise = |acc, nz| {
        a1 = acc.append(bb(nz.enabled)).append(bb(nz.halt))
        a2 = w_env(a1, nz.env).append(bb(nz.short_mode))
        w16(w16(w16(a2, nz.period), nz.timer), nz.lfsr).append(nz.length)
    }

    r_noise = |cur| {
        en = rb(cur)?
        ha = rb(en.cur)?
        ev = r_env(ha.cur)?
        sm = rb(ev.cur)?
        pe = r16(sm.cur)?
        ti = r16(pe.cur)?
        lf = r16(ti.cur)?
        le = r8(lf.cur)?
        Ok({ cur: le.cur, v: {
            enabled: en.v,
            halt: ha.v,
            env: ev.v,
            short_mode: sm.v,
            period: pe.v,
            timer: ti.v,
            lfsr: lf.v,
            length: le.v,
        } })
    }

    w_dmc = |acc, d| {
        a1 = acc.append(bb(d.enabled)).append(bb(d.irq_on)).append(bb(d.loop_flag))
        a2 = w16(w16(a1, d.period), d.timer).append(d.level)
        a3 = w16(w16(w16(w16(a2, d.sample_addr), d.sample_len), d.current_addr), d.bytes_remaining)
        a4 =
            match d.buffer {
                Empty => a3.append(0).append(0)
                Full(b) => a3.append(1).append(b)
            }
        a4.append(d.shift).append(d.bits_remaining).append(bb(d.silence)).append(bb(d.irq_flag))
    }

    r_dmc = |cur| {
        en = rb(cur)?
        io = rb(en.cur)?
        lo = rb(io.cur)?
        pe = r16(lo.cur)?
        ti = r16(pe.cur)?
        lv = r8(ti.cur)?
        sa = r16(lv.cur)?
        sl = r16(sa.cur)?
        ca = r16(sl.cur)?
        br = r16(ca.cur)?
        bt = r8(br.cur)?
        bv = r8(bt.cur)?
        buffer = if bt.v == 0 { Empty } else { Full(bv.v) }
        sh = r8(bv.cur)?
        bi = r8(sh.cur)?
        si = rb(bi.cur)?
        ifl = rb(si.cur)?
        Ok({ cur: ifl.cur, v: {
            enabled: en.v,
            irq_on: io.v,
            loop_flag: lo.v,
            period: pe.v,
            timer: ti.v,
            level: lv.v,
            sample_addr: sa.v,
            sample_len: sl.v,
            current_addr: ca.v,
            bytes_remaining: br.v,
            buffer: buffer,
            shift: sh.v,
            bits_remaining: bi.v,
            silence: si.v,
            irq_flag: ifl.v,
        } })
    }

    w_frame = |acc, f|
        wi64(acc.append(bb(f.mode5)).append(bb(f.inhibit)), f.c).append(bb(f.irq_flag))

    r_frame = |cur| {
        m5 = rb(cur)?
        ih = rb(m5.cur)?
        c = ri64(ih.cur)?
        ifl = rb(c.cur)?
        Ok({ cur: ifl.cur, v: { mode5: m5.v, inhibit: ih.v, c: c.v, irq_flag: ifl.v } })
    }

    # --- mapper state ---

    w_mapper = |acc, cart|
        match cart.mapper {
            Nrom => acc.append(0)
            Uxrom(m) => acc.append(1).append(m.bank)
            Cnrom(m) => acc.append(2).append(m.bank)
            Mmc1(m) => acc.append(3).append(m.shift).append(m.count).append(m.control).append(m.chr0).append(m.chr1).append(m.prg_bank)
            Mmc3(m) =>
                w64(
                    acc.append(4)
                        .append(m.bank_select)
                        .concat(m.banks)
                        .append(m.mirroring)
                        .append(m.irq_latch)
                        .append(m.irq_counter)
                        .append(bb(m.irq_reload))
                        .append(bb(m.irq_enabled))
                        .append(bb(m.irq_asserted))
                        .append(bb(m.a12_level)),
                    m.a12_fell,
                )

            Unsupported => acc.append(5)
            Axrom(m) => acc.append(6).append(m.bank)
            Mmc2(m) =>
                acc.append(9)
                    .append(m.prg_bank)
                    .append(m.chr_fd0)
                    .append(m.chr_fe0)
                    .append(m.chr_fd1)
                    .append(m.chr_fe1)
                    .append(m.latch0)
                    .append(m.latch1)
                    .append(m.mirroring)

            ColorDreams(m) => acc.append(7).append(m.bank)
            Gxrom(m) => acc.append(8).append(m.bank)
        }

    r_mapper = |cur| {
        t = r8(cur)?
        match t.v {
            0 => Ok({ cur: t.cur, v: Nrom })
            1 => {
                b = r8(t.cur)?
                Ok({ cur: b.cur, v: Uxrom({ bank: b.v }) })
            }

            2 => {
                b = r8(t.cur)?
                Ok({ cur: b.cur, v: Cnrom({ bank: b.v }) })
            }

            3 => {
                sh = r8(t.cur)?
                co = r8(sh.cur)?
                ct = r8(co.cur)?
                c0 = r8(ct.cur)?
                c1 = r8(c0.cur)?
                pb = r8(c1.cur)?
                Ok({ cur: pb.cur, v: Mmc1({ shift: sh.v, count: co.v, control: ct.v, chr0: c0.v, chr1: c1.v, prg_bank: pb.v }) })
            }

            4 => {
                bs = r8(t.cur)?
                bk = rbytes(bs.cur, 8)?
                mi = r8(bk.cur)?
                il = r8(mi.cur)?
                ic = r8(il.cur)?
                ir = rb(ic.cur)?
                ie = rb(ir.cur)?
                ia = rb(ie.cur)?
                al = rb(ia.cur)?
                af = r64(al.cur)?
                Ok({ cur: af.cur, v: Mmc3({
                    bank_select: bs.v,
                    banks: bk.v,
                    mirroring: mi.v,
                    irq_latch: il.v,
                    irq_counter: ic.v,
                    irq_reload: ir.v,
                    irq_enabled: ie.v,
                    irq_asserted: ia.v,
                    a12_level: al.v,
                    a12_fell: af.v,
                }) })
            }

            5 => Ok({ cur: t.cur, v: Unsupported })
            6 => {
                b = r8(t.cur)?
                Ok({ cur: b.cur, v: Axrom({ bank: b.v }) })
            }

            7 => {
                b = r8(t.cur)?
                Ok({ cur: b.cur, v: ColorDreams({ bank: b.v }) })
            }

            8 => {
                b = r8(t.cur)?
                Ok({ cur: b.cur, v: Gxrom({ bank: b.v }) })
            }

            9 => {
                pb = r8(t.cur)?
                fd0 = r8(pb.cur)?
                fe0 = r8(fd0.cur)?
                fd1 = r8(fe0.cur)?
                fe1 = r8(fd1.cur)?
                l0 = r8(fe1.cur)?
                l1 = r8(l0.cur)?
                mi = r8(l1.cur)?
                Ok({ cur: mi.cur, v: Mmc2({
                    prg_bank: pb.v,
                    chr_fd0: fd0.v,
                    chr_fe0: fe0.v,
                    chr_fd1: fd1.v,
                    chr_fe1: fe1.v,
                    latch0: l0.v,
                    latch1: l1.v,
                    mirroring: mi.v,
                }) })
            }

            _ => Err(Corrupt)
        }
    }

    # --- encode ---

    encode : Nes -> Try(List(U8), [NotAConsole, ..])
    encode = |nes|
        match nes.cpu.bus {
            Flat(_) => Err(NotAConsole)
            Nrom(n) => {
                reg = nes.cpu.reg
                h0 : List(U8)
                h0 = [0x52, 0x4E, 0x45, 0x53] # "RNES"
                h1 = w64(w16(h0, 4), n.cart.hash)
                c1 = w16(h1, reg.program_counter)
                c2 = c1.append(reg.stack_pointer).append(reg.accumulator).append(reg.x).append(reg.y).append(reg.status)
                c3 = w64(c2, nes.cpu.cycles).append(bb(nes.cpu.jammed)).append(bb(nes.delayed_nmi))
                b1 = w64(c3, n.dma_stall).append(bb(n.strobe)).append(n.shift).append(pack_buttons(n.buttons))
                b2 = b1.concat(n.ram).concat(n.prg_ram)
                p = Box.unbox(n.ppu)
                p1 = b2.append(p.ctrl).append(p.mask).append(p.status).append(p.oam_addr)
                p2 = w16(w16(p1, p.v), p.t).append(p.fine_x).append(bb(p.latch)).append(p.read_buffer).append(mir_tag(p.mirroring))
                p3 = w64(w16(w16(p2, p.dot), p.scanline), p.frame).append(bb(p.nmi_pending))
                p3t0 = w64(w64(w64(p3, p.clock).append(bb(p.odd_frame)).append(bb(p.vbl_suppress)), p.vbl_set_clock), p.render_change_clock).append(bb(p.render_prev))
                p3t = w16(w16(p3t0, p.rendered_x), p.span_v)
                p4 = p3t.concat(p.oam).concat(p.vram).concat(p.palette).concat(p.framebuffer)
                a = Box.unbox(n.apu)
                a1 = w_pulse(w_pulse(p4, a.pulse1), a.pulse2)
                a2 = w_dmc(w_noise(w_triangle(a1, a.triangle), a.noise), a.dmc)
                a3 = w64(w_frame(a2, a.frame).append(bb(a.odd)), a.sample_acc)
                m1 = w_mapper(a3, n.cart)
                Ok(if n.cart.chr_writable { m1.concat(n.cart.chr) } else { m1 })
            }
        }

    # --- decode ---

    decode : List(U8), Cartridge -> Try(Nes, [BadMagic, UnsupportedVersion(U16), RomMismatch, Truncated, Corrupt, ..])
    decode = |bytes, cart| {
        cur0 : Cur
        cur0 = { bytes: bytes, pos: 0 }
        mg = rbytes(cur0, 4)?
        magic_ok = if mg.v == [0x52, 0x4E, 0x45, 0x53] { Ok({}) } else { Err(BadMagic) }
        magic_ok?
        ver = r16(mg.cur)?
        ver_ok = if ver.v == 4 { Ok({}) } else { Err(UnsupportedVersion(ver.v)) }
        ver_ok?
        hh = r64(ver.cur)?
        hash_ok = if hh.v == cart.hash { Ok({}) } else { Err(RomMismatch) }
        hash_ok?
        # CPU + console bookkeeping
        pc = r16(hh.cur)?
        sp = r8(pc.cur)?
        ac = r8(sp.cur)?
        xr = r8(ac.cur)?
        yr = r8(xr.cur)?
        st = r8(yr.cur)?
        cy = r64(st.cur)?
        jm = rb(cy.cur)?
        dn = rb(jm.cur)?
        # bus
        ds = r64(dn.cur)?
        sb = rb(ds.cur)?
        sh = r8(sb.cur)?
        bt = r8(sh.cur)?
        ram = rbytes(bt.cur, 0x0800)?
        prg_ram = rbytes(ram.cur, 0x2000)?
        # PPU
        ct = r8(prg_ram.cur)?
        mk = r8(ct.cur)?
        ps = r8(mk.cur)?
        oa = r8(ps.cur)?
        vv = r16(oa.cur)?
        tt = r16(vv.cur)?
        fx = r8(tt.cur)?
        la = rb(fx.cur)?
        rd = r8(la.cur)?
        mi = r8(rd.cur)?
        mirroring =
            match mi.v {
                0 => Ok(Horizontal)
                1 => Ok(Vertical)
                2 => Ok(FourScreen)
                3 => Ok(SingleLow)
                4 => Ok(SingleHigh)
                _ => Err(Corrupt)
            }
        mir = mirroring?
        dt = r16(mi.cur)?
        sl = r16(dt.cur)?
        fr = r64(sl.cur)?
        np = rb(fr.cur)?
        pclk = r64(np.cur)?
        podd = rb(pclk.cur)?
        psup = rb(podd.cur)?
        pvsc = r64(psup.cur)?
        prcc = r64(pvsc.cur)?
        prpv = rb(prcc.cur)?
        prx = r16(prpv.cur)?
        psv = r16(prx.cur)?
        oam = rbytes(psv.cur, 256)?
        vram = rbytes(oam.cur, 0x0800)?
        pal = rbytes(vram.cur, 32)?
        fb = rbytes(pal.cur, 61440)?
        # APU
        pu1 = r_pulse(fb.cur)?
        pu2 = r_pulse(pu1.cur)?
        tr = r_triangle(pu2.cur)?
        nz = r_noise(tr.cur)?
        dm = r_dmc(nz.cur)?
        fs = r_frame(dm.cur)?
        od = rb(fs.cur)?
        sa = r64(od.cur)?
        # cartridge mutable state
        mp = r_mapper(sa.cur)?
        chr = if cart.chr_writable { rbytes(mp.cur, 0x2000)? } else { { cur: mp.cur, v: cart.chr } }
        len_ok = if chr.cur.pos == bytes.len() { Ok({}) } else { Err(Corrupt) }
        len_ok?
        # reassemble
        cart2 = { ..cart, mapper: mp.v, chr: chr.v }
        ppu2 = { ..Ppu.init(mir),
            ctrl: ct.v,
            mask: mk.v,
            status: ps.v,
            oam_addr: oa.v,
            v: vv.v,
            t: tt.v,
            fine_x: fx.v,
            latch: la.v,
            read_buffer: rd.v,
            oam: oam.v,
            vram: vram.v,
            palette: pal.v,
            dot: dt.v,
            scanline: sl.v,
            frame: fr.v,
            nmi_pending: np.v,
            clock: pclk.v,
            odd_frame: podd.v,
            vbl_suppress: psup.v,
            vbl_set_clock: pvsc.v,
            render_change_clock: prcc.v,
            render_prev: prpv.v,
            rendered_x: prx.v,
            span_v: psv.v,
            framebuffer: fb.v,
        }
        apu2 = { ..Apu.init({}),
            pulse1: pu1.v,
            pulse2: pu2.v,
            triangle: tr.v,
            noise: nz.v,
            dmc: dm.v,
            frame: fs.v,
            odd: od.v,
            sample_acc: sa.v,
            samples: [],
        }
        bus2 =
            match Bus.from_cartridge(cart2) {
                Flat(m) => Flat(m)
                Nrom(fresh) => {
                    mapper_clocks =
                        match cart2.mapper {
                            Mmc3(_) => Bool.True
                            _ => Bool.False
                        }
                    Nrom({ ..fresh,
                        ram: ram.v,
                        prg_ram: prg_ram.v,
                        ppu: Box.box(ppu2),
                        apu: Box.box(apu2),
                        ppu_clock: ppu2.clock,
                        ppu_next_event: Ppu.next_event_after(ppu2, mapper_clocks),
                        ppu_pending_nmi: ppu2.nmi_pending,
                        frame_count: ppu2.frame,
                        dma_stall: ds.v,
                        buttons: unpack_buttons(bt.v),
                        strobe: sb.v,
                        shift: sh.v,
                    })
                }
            }
        reg2 = Register.init({})
            .write16(ProgramCounter, pc.v)
            .write8(StackPointer, sp.v)
            .write8(Accumulator, ac.v)
            .write8(X, xr.v)
            .write8(Y, yr.v)
            .write8(Status, st.v)
        cpu2 = { ..Cpu.make(reg2, bus2), cycles: cy.v, jammed: jm.v }
        Ok({ ..Nes.from_cartridge(cart2), cpu: cpu2, delayed_nmi: dn.v })
    }

    # --- battery-backed PRG RAM (.sav) ---

    battery_backed : Nes -> Bool
    battery_backed = |nes|
        match nes.cpu.bus {
            Flat(_) => Bool.False
            Nrom(n) => n.cart.header.battery
        }

    # the 8 KiB PRG RAM slice, raw - what a frontend persists as <rom>.sav
    battery_ram : Nes -> Try(List(U8), [NotBatteryBacked, ..])
    battery_ram = |nes|
        match nes.cpu.bus {
            Flat(_) => Err(NotBatteryBacked)
            Nrom(n) =>
                if n.cart.header.battery {
                    Ok(n.prg_ram)
                } else {
                    Err(NotBatteryBacked)
                }
        }

    # seed PRG RAM from a previously saved .sav (typically right after
    # Nes.from_cartridge); wrong-sized data is rejected, never padded
    with_battery_ram : Nes, List(U8) -> Try(Nes, [WrongSavSize, NotAConsole, ..])
    with_battery_ram = |nes, sav|
        match nes.cpu.bus {
            Flat(_) => Err(NotAConsole)
            Nrom(n) =>
                if sav.len() == 0x2000 {
                    Ok({ ..nes, cpu: { ..nes.cpu, bus: Nrom({ ..n, prg_ram: sav }) } })
                } else {
                    Err(WrongSavSize)
                }
        }
}

# round-trip: a console mid-run encodes, decodes against the same cartridge,
# and both sides step identically afterwards
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 1, 0x02, 0x00, 0, 0, 0, 0, 0, 0, 0, 0] # battery flag set
    prg0 = List.repeat(0xEA, 16384).set(0x3FFC, 0x00) ?? []
    prg = prg0.set(0x3FFD, 0x80) ?? []
    rom = header.concat(prg).concat(List.repeat(0, 8192))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            run = |n, k| if k == 0 { n } else { run(Nes.step(n), k - 1) }
            k0 : U64
            k0 = 500
            nes = run(Nes.from_cartridge(cart), k0)
            match Snapshot.encode(nes) {
                Ok(bytes) =>
                    match Snapshot.decode(bytes, cart) {
                        Ok(back) => {
                            k1 : U64
                            k1 = 200
                            a = run(nes, k1)
                            b = run(back, k1)
                            a.cpu.reg.program_counter == b.cpu.reg.program_counter
                            and a.cpu.cycles == b.cpu.cycles
                            and a.framebuffer() == b.framebuffer()
                        }

                        Err(_) => Bool.False
                    }

                Err(_) => Bool.False
            }
        }

        Err(_) => Bool.False
    }
}

# wrong ROM, wrong version, truncation, and battery round-trip
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 1, 0x02, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    prg0 = List.repeat(0xEA, 16384).set(0x3FFC, 0x00) ?? []
    prg = prg0.set(0x3FFD, 0x80) ?? []
    rom = header.concat(prg).concat(List.repeat(0, 8192))
    other_rom = header.concat(prg0.set(0x3FFD, 0xC0) ?? []).concat(List.repeat(0, 8192))
    match { a: Cartridge.from_bytes(rom), b: Cartridge.from_bytes(other_rom) } {
        { a: Ok(cart), b: Ok(other) } => {
            nes = Nes.from_cartridge(cart)
            match Snapshot.encode(nes) {
                Ok(bytes) => {
                    wrong_rom =
                        match Snapshot.decode(bytes, other) {
                            Err(RomMismatch) => Bool.True
                            _ => Bool.False
                        }
                    bad_ver =
                        match Snapshot.decode(bytes.set(4, 0xFF) ?? bytes, cart) {
                            Err(UnsupportedVersion(0x00FF)) => Bool.True
                            _ => Bool.False
                        }
                    truncated =
                        match Snapshot.decode(List.sublist(bytes, { start: 0, len: bytes.len() - 1 }), cart) {
                            Err(Truncated) => Bool.True
                            Err(Corrupt) => Bool.True
                            _ => Bool.False
                        }
                    sav = List.repeat(0x5A, 0x2000)
                    battery =
                        match Snapshot.with_battery_ram(nes, sav) {
                            Ok(seeded) =>
                                seeded.cpu.bus.read8(0x6000).value == 0x5A
                                and match Snapshot.battery_ram(seeded) {
                                    Ok(out) => out == sav
                                    Err(_) => Bool.False
                                }

                            Err(_) => Bool.False
                        }
                    wrong_size =
                        match Snapshot.with_battery_ram(nes, [1, 2, 3]) {
                            Err(WrongSavSize) => Bool.True
                            _ => Bool.False
                        }
                    wrong_rom and bad_ver and truncated and battery and wrong_size
                }

                Err(_) => Bool.False
            }
        }

        _ => Bool.False
    }
}

# mapper tags 6/7/8/9 (AxROM / ColorDreams / GxROM / MMC2) round-trip the mapper section
expect {
    prg = List.repeat(0, 32768).concat(List.repeat(1, 32768))
    chr = List.repeat(0, 8192).concat(List.repeat(1, 8192))
    ax_rom = [0x4E, 0x45, 0x53, 0x1A, 4, 0, 0x70, 0x00, 0, 0, 0, 0, 0, 0, 0, 0].concat(prg)
    cd_rom = [0x4E, 0x45, 0x53, 0x1A, 4, 2, 0xB0, 0x00, 0, 0, 0, 0, 0, 0, 0, 0].concat(prg).concat(chr)
    gx_rom = [0x4E, 0x45, 0x53, 0x1A, 4, 2, 0x20, 0x40, 0, 0, 0, 0, 0, 0, 0, 0].concat(prg).concat(chr)
    mm_rom = [0x4E, 0x45, 0x53, 0x1A, 4, 2, 0x90, 0x00, 0, 0, 0, 0, 0, 0, 0, 0].concat(prg).concat(chr)
    check = |rom, state|
        match Cartridge.from_bytes(rom) {
            Ok(cart) =>
                match Snapshot.encode(Nes.from_cartridge({ ..cart, mapper: state })) {
                    Ok(bytes) =>
                        match Snapshot.decode(bytes, cart) {
                            Ok(back) =>
                                match back.cpu.bus {
                                    Nrom(n) => n.cart.mapper == state
                                    _ => Bool.False
                                }

                            Err(_) => Bool.False
                        }

                    Err(_) => Bool.False
                }

            Err(_) => Bool.False
        }
    check(ax_rom, Axrom({ bank: 0x13 }))
    and check(cd_rom, ColorDreams({ bank: 0x21 }))
    and check(gx_rom, Gxrom({ bank: 0x12 }))
    and check(mm_rom, Mmc2({ prg_bank: 5, chr_fd0: 1, chr_fe0: 2, chr_fd1: 3, chr_fe1: 4, latch0: 0xFD, latch1: 0xFE, mirroring: 1 }))
}
