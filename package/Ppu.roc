import /Cartridge

# NTSC PPU (2C02): CPU-visible registers with their read/write effects,
# the PPU's own address space (CHR pattern tables, nametable VRAM with
# cartridge mirroring, palette RAM), frame timing (341 dots x 262 scanlines,
# vblank at 241, pre-render at 261), and scanline rendering into a 256x240
# framebuffer of NES palette indices.
#
# Timing is scanline-granular: `tick` consumes dots boundary-by-boundary and
# a visible scanline is rendered in full when the counter leaves it.
# References: https://www.nesdev.org/wiki/PPU_scrolling
#             https://www.nesdev.org/wiki/PPU_registers
Ppu := {
    # registers
    ctrl : U8, # $2000
    mask : U8, # $2001
    status : U8, # $2002 bits 5-7: overflow, sprite-0 hit, vblank
    oam_addr : U8, # $2003
    # loopy internals
    v : U16, # current VRAM address (15 bits)
    t : U16, # temporary VRAM address
    fine_x : U8,
    latch : Bool, # shared $2005/$2006 write toggle (w)
    read_buffer : U8, # $2007 buffered reads
    # memories
    oam : List(U8), # 256 bytes
    vram : List(U8), # 2 KiB nametables
    palette : List(U8), # 32 bytes
    mirroring : [Horizontal, Vertical, FourScreen, SingleLow, SingleHigh],
    # timing
    dot : U16, # 0-340
    scanline : U16, # 0-261 (261 = pre-render)
    frame : U64, # increments at each vblank entry
    nmi_pending : Bool, # latched at vblank entry when NMI enabled; consumed by Nes
    # output
    framebuffer : List(U8), # 256*240 NES palette indices
}.{
    init : [Horizontal, Vertical, FourScreen, SingleLow, SingleHigh] -> Ppu
    init = |mirroring| {
        ctrl: 0,
        mask: 0,
        status: 0,
        oam_addr: 0,
        v: 0,
        t: 0,
        fine_x: 0,
        latch: Bool.False,
        read_buffer: 0,
        oam: List.repeat(0, 256),
        vram: List.repeat(0, 0x0800),
        palette: List.repeat(0, 32),
        mirroring: mirroring,
        dot: 0,
        scanline: 0,
        frame: 0,
        nmi_pending: Bool.False,
        framebuffer: List.repeat(0, 61440),
    }

    # --- PPU address space ---

    # nametable address -> physical index into the 2 KiB VRAM
    mirror_nt : Ppu, U16 -> U64
    mirror_nt = |ppu, addr| {
        rel = addr.bitwise_and(0x0FFF)
        table = rel.shr_zf_wrap(10) # 0-3
        offset = rel.bitwise_and(0x03FF)
        physical =
            match ppu.mirroring {
                Vertical => table.bitwise_and(1)
                Horizontal => table.shr_zf_wrap(1)
                FourScreen => table.bitwise_and(1) # unsupported; treated as vertical
                SingleLow => 0
                SingleHigh => 1
            }
        physical.shl_wrap(10).bitwise_or(offset).to_u64()
    }

    pal_index : U16 -> U64
    pal_index = |addr| {
        rel = addr.bitwise_and(0x001F)
        # $3F10/$3F14/$3F18/$3F1C mirror $3F00/$3F04/$3F08/$3F0C
        mirrored =
            if rel >= 0x10 and rel.bitwise_and(0x03) == 0 {
                rel.bitwise_and(0x0F)
            } else {
                rel
            }
        mirrored.to_u64()
    }

    ppu_read : Ppu, Cartridge, U16 -> U8
    ppu_read = |ppu, cart, addr0| {
        addr = addr0.bitwise_and(0x3FFF)
        if addr < 0x2000 {
            cart.read_chr(addr)
        } else if addr < 0x3F00 {
            ppu.vram.get(mirror_nt(ppu, addr)) ?? 0
        } else {
            ppu.palette.get(pal_index(addr)) ?? 0
        }
    }

    ppu_write : Ppu, U16, U8 -> Ppu
    ppu_write = |ppu, addr0, value| {
        addr = addr0.bitwise_and(0x3FFF)
        if addr < 0x2000 {
            ppu # CHR ROM; CHR RAM arrives with the mappers change
        } else if addr < 0x3F00 {
            { ..ppu, vram: ppu.vram.set(mirror_nt(ppu, addr), value) ?? ppu.vram }
        } else {
            { ..ppu, palette: ppu.palette.set(pal_index(addr), value) ?? ppu.palette }
        }
    }

    set_mirroring : Ppu, [Horizontal, Vertical, FourScreen, SingleLow, SingleHigh] -> Ppu
    set_mirroring = |ppu, mirroring| { ..ppu, mirroring: mirroring }

    vram_increment : Ppu -> U16
    vram_increment = |ppu| if ppu.ctrl.bitwise_and(0x04) != 0 { 32 } else { 1 }

    # --- CPU-visible registers (addr already mirrored to 0-7 by the bus) ---

    read_reg : Ppu, Cartridge, U16 -> { ppu : Ppu, value : U8 }
    read_reg = |ppu, cart, reg|
        match reg {
            2 => {
                # PPUSTATUS: read clears vblank and the write latch
                value = ppu.status.bitwise_and(0xE0).bitwise_or(ppu.read_buffer.bitwise_and(0x1F))
                { ppu: { ..ppu, status: ppu.status.bitwise_and(0x7F), latch: Bool.False }, value: value }
            }

            4 => { ppu: ppu, value: ppu.oam.get(ppu.oam_addr.to_u64()) ?? 0 }
            7 => {
                data = ppu_read(ppu, cart, ppu.v)
                addr = ppu.v.bitwise_and(0x3FFF)
                next_v = ppu.v.plus_wrap(vram_increment(ppu))
                if addr >= 0x3F00 {
                    # palette reads are direct; the buffer refills from the nametable underneath
                    under = ppu_read(ppu, cart, addr.bitwise_and(0x2FFF))
                    { ppu: { ..ppu, read_buffer: under, v: next_v }, value: data }
                } else {
                    { ppu: { ..ppu, read_buffer: data, v: next_v }, value: ppu.read_buffer }
                }
            }

            _ => { ppu: ppu, value: 0 }
        }

    # chr_write reports a PPUDATA write landing in pattern space (CHR RAM)
    # for the bus to apply to the cartridge
    write_reg : Ppu, U16, U8 -> { ppu : Ppu, chr_write : [NoChr, ChrAt(U16, U8)] }
    write_reg = |ppu, reg, value|
        match reg {
            0 => {
                # PPUCTRL: nametable select feeds loopy t bits 10-11.
                # Enabling NMI while the vblank flag is set fires one immediately.
                new_t = ppu.t.bitwise_and(0xF3FF).bitwise_or(value.bitwise_and(0x03).to_u16().shl_wrap(10))
                rising = ppu.ctrl.bitwise_and(0x80) == 0 and value.bitwise_and(0x80) != 0
                fire = rising and ppu.status.bitwise_and(0x80) != 0
                { ppu: { ..ppu, ctrl: value, t: new_t, nmi_pending: ppu.nmi_pending or fire }, chr_write: NoChr }
            }

            1 => { ppu: { ..ppu, mask: value }, chr_write: NoChr }
            3 => { ppu: { ..ppu, oam_addr: value }, chr_write: NoChr }
            4 => {
                { ppu: { ..ppu,
                    oam: ppu.oam.set(ppu.oam_addr.to_u64(), value) ?? ppu.oam,
                    oam_addr: ppu.oam_addr.plus_wrap(1),
                }, chr_write: NoChr }
            }

            5 =>
                if ppu.latch == Bool.False {
                    # first write: coarse X + fine X
                    new_t = ppu.t.bitwise_and(0xFFE0).bitwise_or(value.shr_zf_wrap(3).to_u16())
                    { ppu: { ..ppu, t: new_t, fine_x: value.bitwise_and(0x07), latch: Bool.True }, chr_write: NoChr }
                } else {
                    # second write: coarse Y + fine Y
                    coarse_y = value.bitwise_and(0xF8).to_u16().shl_wrap(2)
                    fine_y = value.bitwise_and(0x07).to_u16().shl_wrap(12)
                    { ppu: { ..ppu, t: ppu.t.bitwise_and(0x8C1F).bitwise_or(coarse_y).bitwise_or(fine_y), latch: Bool.False }, chr_write: NoChr }
                }

            6 =>
                if ppu.latch == Bool.False {
                    new_t = ppu.t.bitwise_and(0x00FF).bitwise_or(value.bitwise_and(0x3F).to_u16().shl_wrap(8))
                    { ppu: { ..ppu, t: new_t, latch: Bool.True }, chr_write: NoChr }
                } else {
                    new_t = ppu.t.bitwise_and(0xFF00).bitwise_or(value.to_u16())
                    { ppu: { ..ppu, t: new_t, v: new_t, latch: Bool.False }, chr_write: NoChr }
                }

            7 => {
                addr = ppu.v.bitwise_and(0x3FFF)
                advanced = { ..ppu, v: ppu.v.plus_wrap(vram_increment(ppu)) }
                if addr < 0x2000 {
                    { ppu: advanced, chr_write: ChrAt(addr, value) }
                } else {
                    { ppu: ppu_write(advanced, addr, value), chr_write: NoChr }
                }
            }

            _ => { ppu: ppu, chr_write: NoChr }
        }

    rendering_enabled : Ppu -> Bool
    rendering_enabled = |ppu| ppu.mask.bitwise_and(0x18) != 0

    # --- loopy per-scanline movement ---

    increment_y : U16 -> U16
    increment_y = |v|
        if v.bitwise_and(0x7000) != 0x7000 {
            v.plus_wrap(0x1000) # fine Y++
        } else {
            base = v.bitwise_and(0x8FFF)
            coarse_y = base.shr_zf_wrap(5).bitwise_and(0x1F)
            if coarse_y == 29 {
                # wrap into the next vertical nametable
                base.bitwise_and(0xFC1F).bitwise_xor(0x0800)
            } else if coarse_y == 31 {
                base.bitwise_and(0xFC1F)
            } else {
                base.bitwise_and(0xFC1F).bitwise_or(coarse_y.plus_wrap(1).shl_wrap(5))
            }
        }

    copy_horizontal : U16, U16 -> U16
    copy_horizontal = |v, t| v.bitwise_and(0xFBE0).bitwise_or(t.bitwise_and(0x041F))

    copy_vertical : U16, U16 -> U16
    copy_vertical = |v, t| v.bitwise_and(0x841F).bitwise_or(t.bitwise_and(0x7BE0))

    # --- background ---

    # one background pixel row of 264 entries (33 tiles); fine_x trims it
    bg_row : Ppu, Cartridge -> List(U8)
    bg_row = |ppu, cart| {
        pattern_base : U16
        pattern_base = if ppu.ctrl.bitwise_and(0x10) != 0 { 0x1000 } else { 0x0000 }
        fine_y = ppu.v.shr_zf_wrap(12).bitwise_and(0x07)
        walk = |vv, tile_i, acc| {
            if tile_i >= 33 {
                acc
            } else {
                nt_addr = vv.bitwise_and(0x0FFF).bitwise_or(0x2000)
                tile = ppu_read(ppu, cart, nt_addr)
                attr_addr = vv.bitwise_and(0x0C00).bitwise_or(0x23C0).bitwise_or(vv.shr_zf_wrap(4).bitwise_and(0x38)).bitwise_or(vv.shr_zf_wrap(2).bitwise_and(0x07))
                attr = ppu_read(ppu, cart, attr_addr)
                quad_shift = vv.shr_zf_wrap(4).bitwise_and(0x04).bitwise_or(vv.bitwise_and(0x02)).to_u8_wrap()
                pal2 = attr.shr_zf_wrap(quad_shift).bitwise_and(0x03)
                row_addr = pattern_base.plus_wrap(tile.to_u16().shl_wrap(4)).plus_wrap(fine_y)
                lo = ppu_read(ppu, cart, row_addr)
                hi = ppu_read(ppu, cart, row_addr.plus_wrap(8))
                px = |bit| {
                    p0 = lo.shr_zf_wrap(bit).bitwise_and(1)
                    p1 = hi.shr_zf_wrap(bit).bitwise_and(1)
                    p1.shl_wrap(1).bitwise_or(p0) # 0-3 pattern value
                }
                # pack pattern + palette into one byte: pal2*4 + pattern (0 stays 0 = transparent)
                tile_px = [px(7), px(6), px(5), px(4), px(3), px(2), px(1), px(0)].map(|p| if p == 0 { 0 } else { pal2.shl_wrap(2).bitwise_or(p) })
                # coarse X increment with nametable-x wrap
                next_v =
                    if vv.bitwise_and(0x001F) == 31 {
                        vv.bitwise_and(0xFFE0).bitwise_xor(0x0400)
                    } else {
                        vv.plus_wrap(1)
                    }
                walk(next_v, tile_i.plus(1), acc.concat(tile_px))
            }
        }
        row : List(U8)
        row = walk(ppu.v, 0, [])
        List.sublist(row, { start: ppu.fine_x.to_u64(), len: 256 })
    }

    # --- sprites ---

    sprite_height : Ppu -> U16
    sprite_height = |ppu| if ppu.ctrl.bitwise_and(0x20) != 0 { 16 } else { 8 }

    # per-pixel sprite row: 0 = transparent, else packed
    # bit7 = sprite0, bit6 = behind background, bits 0-4 = palette entry (0x10 + pal*4 + pattern)
    SpritePixel : { color : U8, behind : Bool, is_zero : Bool }

    sprite_row : Ppu, Cartridge, U16 -> List([Empty, Px(SpritePixel)])
    sprite_row = |ppu, cart, line| {
        height = sprite_height(ppu)
        scan = |idx, found, acc| {
            if idx >= 64 or found >= 8 {
                acc
            } else {
                base = idx.shl_wrap(2).to_u64()
                oam_y = ppu.oam.get(base) ?? 0xFF
                top = oam_y.to_u16().plus_wrap(1)
                row = line.minus_wrap(top)
                if line >= top and row < height {
                    tile = ppu.oam.get(base.plus(1)) ?? 0
                    attr = ppu.oam.get(base.plus(2)) ?? 0
                    sx = ppu.oam.get(base.plus(3)) ?? 0
                    vrow = if attr.bitwise_and(0x80) != 0 { height.minus_wrap(1).minus_wrap(row) } else { row }
                    pattern_addr =
                        if height == 16 {
                            bank : U16
                            bank = if tile.bitwise_and(0x01) != 0 { 0x1000 } else { 0x0000 }
                            tile16 = tile.bitwise_and(0xFE).to_u16()
                            fine = vrow.bitwise_and(0x07)
                            second = if vrow >= 8 { 16 } else { 0 }
                            bank.plus_wrap(tile16.shl_wrap(4)).plus_wrap(second).plus_wrap(fine)
                        } else {
                            bank : U16
                            bank = if ppu.ctrl.bitwise_and(0x08) != 0 { 0x1000 } else { 0x0000 }
                            bank.plus_wrap(tile.to_u16().shl_wrap(4)).plus_wrap(vrow)
                        }
                    lo = ppu_read(ppu, cart, pattern_addr)
                    hi = ppu_read(ppu, cart, pattern_addr.plus_wrap(8))
                    pal = attr.bitwise_and(0x03)
                    behind = attr.bitwise_and(0x20) != 0
                    hflip = attr.bitwise_and(0x40) != 0
                    place = |acc2, i| {
                        if i >= 8 {
                            acc2
                        } else {
                            bit = if hflip { i.to_u8_wrap() } else { 7 - i.to_u8_wrap() }
                            p0 = lo.shr_zf_wrap(bit).bitwise_and(1)
                            p1 = hi.shr_zf_wrap(bit).bitwise_and(1)
                            p = p1.shl_wrap(1).bitwise_or(p0)
                            x = sx.to_u64().plus(i)
                            if p == 0 or x > 255 {
                                place(acc2, i.plus(1))
                            } else {
                                # first (lowest OAM index) opaque sprite wins
                                existing = acc2.get(x) ?? Empty
                                match existing {
                                    Px(_) => place(acc2, i.plus(1))
                                    Empty => {
                                        color = pal.shl_wrap(2).bitwise_or(p).bitwise_or(0x10)
                                        px = Px({ color: color, behind: behind, is_zero: idx == 0 })
                                        place(acc2.set(x, px) ?? acc2, i.plus(1))
                                    }
                                }
                            }
                        }
                    }
                    scan(idx.plus_wrap(1), found.plus(1), place(acc, 0))
                } else {
                    scan(idx.plus_wrap(1), found, acc)
                }
            }
        }
        empty : List([Empty, Px(SpritePixel)])
        empty = List.repeat(Empty, 256)
        idx0 : U16
        idx0 = 0
        found0 : U64
        found0 = 0
        scan(idx0, found0, empty)
    }

    # count sprites intersecting the line for the (simplified) overflow flag
    sprites_on_line : Ppu, U16 -> U64
    sprites_on_line = |ppu, line| {
        height = sprite_height(ppu)
        count = |idx, n| {
            if idx >= 64 {
                n
            } else {
                oam_y = ppu.oam.get(idx.shl_wrap(2).to_u64()) ?? 0xFF
                top = oam_y.to_u16().plus_wrap(1)
                row = line.minus_wrap(top)
                if line >= top and row < height {
                    count(idx.plus_wrap(1), n.plus(1))
                } else {
                    count(idx.plus_wrap(1), n)
                }
            }
        }
        idx0 : U16
        idx0 = 0
        n0 : U64
        n0 = 0
        count(idx0, n0)
    }

    # --- scanline render ---

    render_scanline : Ppu, Cartridge, U16 -> Ppu
    render_scanline = |ppu, cart, line| {
        backdrop = ppu.palette.get(0) ?? 0
        row_start = line.to_u64().shl_wrap(8) # line * 256
        if rendering_enabled(ppu) == Bool.False {
            fill = |fb, x| if x >= 256 { fb } else { fill(fb.set(row_start.plus(x), backdrop.bitwise_and(0x3F)) ?? fb, x.plus(1)) }
            { ..ppu, framebuffer: fill(ppu.framebuffer, 0) }
        } else {
            show_bg = ppu.mask.bitwise_and(0x08) != 0
            show_sp = ppu.mask.bitwise_and(0x10) != 0
            show_bg_left = ppu.mask.bitwise_and(0x02) != 0
            show_sp_left = ppu.mask.bitwise_and(0x04) != 0
            bg = if show_bg { bg_row(ppu, cart) } else { List.repeat(0, 256) }
            sp = if show_sp { sprite_row(ppu, cart, line) } else { List.repeat(Empty, 256) }
            over = sprites_on_line(ppu, line) > 8
            merge = |st, x| {
                if x >= 256 {
                    st
                } else {
                    bg_entry = bg.get(x) ?? 0
                    bg_opaque0 = bg_entry.bitwise_and(0x03) != 0
                    bg_opaque = bg_opaque0 and (x >= 8 or show_bg_left)
                    sp_entry = if x >= 8 or show_sp_left { sp.get(x) ?? Empty } else { Empty }
                    merged =
                        match sp_entry {
                            Empty => { color: if bg_opaque { bg_entry } else { 0 }, hit: Bool.False }
                            Px(s) =>
                                if bg_opaque and s.behind {
                                    { color: bg_entry, hit: s.is_zero and x != 255 }
                                } else if bg_opaque {
                                    { color: s.color, hit: s.is_zero and x != 255 }
                                } else {
                                    { color: s.color, hit: Bool.False }
                                }
                        }
                    pal_addr = if merged.color == 0 { 0 } else { merged.color.to_u16().bitwise_or(0x3F00) }
                    color = ppu_read(ppu, cart, pal_addr.bitwise_or(0x3F00)).bitwise_and(0x3F)
                    fb2 = st.fb.set(row_start.plus(x), color) ?? st.fb
                    merge({ fb: fb2, hit: st.hit or merged.hit }, x.plus(1))
                }
            }
            result = merge({ fb: ppu.framebuffer, hit: Bool.False }, 0)
            new_status =
                if result.hit and show_bg and show_sp {
                    ppu.status.bitwise_or(0x40)
                } else {
                    ppu.status
                }
            with_over = if over { new_status.bitwise_or(0x20) } else { new_status }
            # loopy end-of-line movement: increment Y, copy horizontal from t
            new_v = copy_horizontal(increment_y(ppu.v), ppu.t)
            { ..ppu, framebuffer: result.fb, status: with_over, v: new_v }
        }
    }

    # --- timing ---

    # advance by `dots`, never skipping a scanline boundary; sl_clocks counts
    # visible/pre-render scanlines completed while rendering was enabled
    # (the scanline approximation of MMC3's A12 clocking)
    tick : Ppu, Cartridge, U64 -> { ppu : Ppu, sl_clocks : U64 }
    tick = |ppu0, cart, dots| tick_go(ppu0, cart, dots, 0)

    tick_go : Ppu, Cartridge, U64, U64 -> { ppu : Ppu, sl_clocks : U64 }
    tick_go = |ppu0, cart, dots, clocks| {
        if dots == 0 {
            { ppu: ppu0, sl_clocks: clocks }
        } else {
            remaining_in_line = 341 - ppu0.dot.to_u64()
            if dots < remaining_in_line {
                { ppu: { ..ppu0, dot: ppu0.dot.plus_wrap(dots.to_u16_wrap()) }, sl_clocks: clocks }
            } else {
                # finish this scanline
                line = ppu0.scanline
                stepped =
                    if line < 240 {
                        render_scanline(ppu0, cart, line)
                    } else {
                        ppu0
                    }
                advanced = { ..stepped, dot: 0, scanline: line.plus_wrap(1) }
                entered =
                    if advanced.scanline == 241 {
                        # vblank entry
                        with_vbl = { ..advanced,
                            status: advanced.status.bitwise_or(0x80),
                            frame: advanced.frame.plus(1),
                        }
                        if with_vbl.ctrl.bitwise_and(0x80) != 0 {
                            { ..with_vbl, nmi_pending: Bool.True }
                        } else {
                            with_vbl
                        }
                    } else if advanced.scanline == 262 {
                        # pre-render (261) ended: clear flags, copy vertical, wrap to line 0
                        cleared = { ..advanced,
                            scanline: 0,
                            status: advanced.status.bitwise_and(0x1F),
                        }
                        if rendering_enabled(cleared) {
                            { ..cleared, v: copy_vertical(cleared.v, cleared.t) }
                        } else {
                            cleared
                        }
                    } else {
                        advanced
                    }
                clocked =
                    if (line < 240 or line == 261) and rendering_enabled(ppu0) {
                        clocks.plus(1)
                    } else {
                        clocks
                    }
                tick_go(entered, cart, dots.minus(remaining_in_line), clocked)
            }
        }
    }

    # DMA landing: 256 bytes written through OAMADDR, wrapping
    load_oam : Ppu, List(U8) -> Ppu
    load_oam = |ppu, data| {
        result = data.fold({ oam: ppu.oam, i: 0 }, |st, byte| {
            dst = ppu.oam_addr.plus_wrap(st.i.to_u8_wrap()).to_u64()
            { oam: st.oam.set(dst, byte) ?? st.oam, i: st.i.plus(1) }
        })
        { ..ppu, oam: result.oam }
    }

    take_nmi : Ppu -> { ppu : Ppu, value : Bool }
    take_nmi = |ppu|
        if ppu.nmi_pending {
            { ppu: { ..ppu, nmi_pending: Bool.False }, value: Bool.True }
        } else {
            { ppu: ppu, value: Bool.False }
        }
}

# $2005/$2006 share the write latch; $2002 reads reset it
expect {
    w = |p, r, v| p.write_reg(r, v).ppu
    p1 = w(w(Ppu.init(Vertical), 6, 0x23), 6, 0x45)
    p1.v == 0x2345 and p1.latch == Bool.False
}

# PPUSTATUS read clears vblank and the latch
expect {
    p0 = Ppu.init(Vertical)
    with_vbl = { ..p0, status: 0x80, latch: Bool.True }
    dummy_cart = Cartridge.from_bytes([0x4E, 0x45, 0x53, 0x1A, 1, 1].concat(List.repeat(0, 10)).concat(List.repeat(0, 24576)))
    match dummy_cart {
        Ok(cart) => {
            r = with_vbl.read_reg(cart, 2)
            r.value.bitwise_and(0x80) != 0 and r.ppu.status.bitwise_and(0x80) == 0 and r.ppu.latch == Bool.False
        }

        Err(_) => Bool.False
    }
}

# PPUDATA reads are buffered for non-palette addresses; address increments
expect {
    dummy_cart = Cartridge.from_bytes([0x4E, 0x45, 0x53, 0x1A, 1, 1].concat(List.repeat(0, 10)).concat(List.repeat(0, 24576)))
    match dummy_cart {
        Ok(cart) => {
            w = |p, r, v| p.write_reg(r, v).ppu
            p1 = w(w(w(Ppu.init(Vertical), 6, 0x20), 6, 0x05), 7, 0x99) # write $2005 = 0x99
            p2 = w(w(p1, 6, 0x20), 6, 0x05) # point back
            r1 = p2.read_reg(cart, 7) # stale buffer
            r2 = r1.ppu.read_reg(cart, 7) # real value
            r1.value == 0 and r2.value == 0x99 and r2.ppu.v == 0x2007
        }

        Err(_) => Bool.False
    }
}

# vertical mirroring maps $2000/$2800 together; palette $3F10 mirrors $3F00
expect {
    w = |p, r, v| p.write_reg(r, v).ppu
    p1 = w(w(w(Ppu.init(Vertical), 6, 0x20), 6, 0x11), 7, 0x42)
    idx_a = Ppu.mirror_nt(p1, 0x2011)
    idx_b = Ppu.mirror_nt(p1, 0x2811)
    p2 = w(w(w(p1, 6, 0x3F), 6, 0x10), 7, 0x2A)
    (p1.vram.get(idx_a) ?? 0) == 0x42 and idx_a == idx_b and (p2.palette.get(0) ?? 0) == 0x2A
}

# vblank sets at scanline 241 entry, clears at pre-render end; NMI latches when enabled
expect {
    dummy_cart = Cartridge.from_bytes([0x4E, 0x45, 0x53, 0x1A, 1, 1].concat(List.repeat(0, 10)).concat(List.repeat(0, 24576)))
    match dummy_cart {
        Ok(cart) => {
            enabled = Ppu.init(Vertical).write_reg(0, 0x80).ppu
            at_vbl = enabled.tick(cart, 82182).ppu # 241 * 341 + 1
            taken = at_vbl.take_nmi()
            after_frame = taken.ppu.tick(cart, 7161).ppu # 21 * 341
            at_vbl.status.bitwise_and(0x80) != 0
            and taken.value == Bool.True
            and after_frame.status.bitwise_and(0x80) == 0
            and after_frame.scanline == 0
        }

        Err(_) => Bool.False
    }
}
