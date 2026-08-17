import /Cartridge

# NTSC PPU (2C02): CPU-visible registers with their read/write effects,
# the PPU's own address space (CHR pattern tables, nametable VRAM with
# cartridge mirroring, palette RAM), frame timing (341 dots x 262 scanlines,
# vblank at 241, pre-render at 261), and scanline rendering into a 256x240
# framebuffer of NES palette indices.
#
# Timing is dot-exact at every observation point via lazy catch-up: the
# console materializes the PPU only at scheduled events (the NMI edge at
# scanline 241 dot 1, mapper scanline clocks) and at register access,
# which syncs to the access's exact dot first. Rendering stays batched
# per scanline, produced when a catch-up completes the line. Dot-level
# behaviors modeled: vblank set/clear at dot 1, the $2002 suppression
# race, NMI enable/disable edge windows, the odd-frame dot skip with the
# ~2-dot $2001 latency (all pinned by blargg's ppu_vbl_nmi singles 01-10).
# References: https://www.nesdev.org/wiki/PPU_scrolling
#             https://www.nesdev.org/wiki/PPU_registers
#             https://www.nesdev.org/wiki/NMI
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
    clock : U64, # absolute dots since power-on (the synced position)
    odd_frame : Bool, # current frame's parity (odd frames skip pre-render dot 339 while rendering)
    render_change_clock : U64, # clock of the last $2001 write that toggled rendering on/off
    render_prev : Bool, # rendering state before that toggle (mask writes take ~2 dots to act)
    vbl_suppress : Bool, # $2002 read one dot before set: this frame's set + NMI are skipped
    vbl_set_clock : U64, # clock at which the vblank flag last set (the $2002 race window)
    # span rendering (mid-scanline splits): pixels of the current visible
    # line already committed, and the live fetch position the next span
    # walks from (advanced per span; overwritten when a $2006/$2007 access
    # loads v - which is how raster splits repoint the right of a line)
    rendered_x : U16,
    span_v : U16,
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
        clock: 0,
        odd_frame: Bool.False,
        render_change_clock: 0,
        render_prev: Bool.False,
        vbl_suppress: Bool.False,
        vbl_set_clock: 0,
        rendered_x: 0,
        span_v: 0,
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

    # a12 reports the PPU address bus's bit-12 level after a $2006/$2007
    # access, for the MMC3's edge detector - only while the PPU is not
    # rendering (the fetch pattern owns the bus during rendering)
    a12_of_v : Ppu, U16 -> [KeepA12, A12(Bool)]
    a12_of_v = |ppu, new_v| {
        in_vblank = ppu.scanline >= 240 and ppu.scanline <= 260
        if rendering_enabled(ppu) == Bool.False or in_vblank {
            A12(new_v.bitwise_and(0x1000) != 0)
        } else {
            KeepA12
        }
    }

    # latches: the MMC2 CHR latch state after the access ($2007 reads through
    # the pattern tables trigger latch flips like rendering fetches do);
    # committed-state pass-through for every other mapper and register
    read_reg : Ppu, Cartridge, U16 -> { ppu : Ppu, value : U8, a12 : [KeepA12, A12(Bool)], latches : { l0 : U8, l1 : U8 } }
    read_reg = |ppu, cart, reg|
        match reg {
            2 => {
                # PPUSTATUS: read clears vblank and the write latch.
                # The $2002 race (the caller synced us to the read's dot):
                # reading one dot before vblank set means the flag never
                # sets and the NMI is skipped this frame; reading on the
                # set dot or one later returns it set but kills the NMI.
                value = ppu.status.bitwise_and(0xE0).bitwise_or(ppu.read_buffer.bitwise_and(0x1F))
                about_to_set = dots_until_vbl_set(ppu) == 1
                just_set =
                    ppu.status.bitwise_and(0x80) != 0
                    and ppu.clock <= ppu.vbl_set_clock.plus(1)
                { ppu: { ..ppu,
                    status: ppu.status.bitwise_and(0x7F),
                    latch: Bool.False,
                    vbl_suppress: ppu.vbl_suppress or about_to_set,
                    nmi_pending: if just_set { Bool.False } else { ppu.nmi_pending },
                }, value: value, a12: KeepA12, latches: cart.chr_latches() }
            }

            4 => { ppu: ppu, value: ppu.oam.get(ppu.oam_addr.to_u64()) ?? 0, a12: KeepA12, latches: cart.chr_latches() }
            7 => {
                data = ppu_read(ppu, cart, ppu.v)
                addr = ppu.v.bitwise_and(0x3FFF)
                next_v = ppu.v.plus_wrap(vram_increment(ppu))
                lat =
                    if addr < 0x2000 {
                        cart.chr_fetch(cart.chr_latches(), addr).latches
                    } else {
                        cart.chr_latches()
                    }
                if addr >= 0x3F00 {
                    # palette reads are direct; the buffer refills from the nametable underneath
                    under = ppu_read(ppu, cart, addr.bitwise_and(0x2FFF))
                    { ppu: { ..ppu, read_buffer: under, v: next_v, span_v: next_v }, value: data, a12: a12_of_v(ppu, next_v), latches: lat }
                } else {
                    { ppu: { ..ppu, read_buffer: data, v: next_v, span_v: next_v }, value: ppu.read_buffer, a12: a12_of_v(ppu, next_v), latches: lat }
                }
            }

            _ => { ppu: ppu, value: 0, a12: KeepA12, latches: cart.chr_latches() }
        }

    # chr_write reports a PPUDATA write landing in pattern space (CHR RAM)
    # for the bus to apply to the cartridge
    write_reg : Ppu, U16, U8 -> { ppu : Ppu, chr_write : [NoChr, ChrAt(U16, U8)], a12 : [KeepA12, A12(Bool)] }
    write_reg = |ppu, reg, value|
        match reg {
            0 => {
                # PPUCTRL: nametable select feeds loopy t bits 10-11.
                # Enabling NMI while the vblank flag is set fires one
                # immediately. Disabling cancels a pending NMI only inside
                # the ~2-dot window after the set - before the CPU's edge
                # detector has latched it; a committed edge fires regardless
                # (blargg 08-nmi_off_timing pins both sides).
                new_t = ppu.t.bitwise_and(0xF3FF).bitwise_or(value.bitwise_and(0x03).to_u16().shl_wrap(10))
                rising = ppu.ctrl.bitwise_and(0x80) == 0 and value.bitwise_and(0x80) != 0
                fire = rising and ppu.status.bitwise_and(0x80) != 0
                cancel =
                    value.bitwise_and(0x80) == 0
                    and ppu.nmi_pending
                    and ppu.clock <= ppu.vbl_set_clock.plus(2)
                pend = if cancel { Bool.False } else { ppu.nmi_pending or fire }
                { ppu: { ..ppu, ctrl: value, t: new_t, nmi_pending: pend }, chr_write: NoChr, a12: KeepA12 }
            }

            1 => {
                # track rendering on/off toggles: the skip decision at
                # pre-render dot 339 sees mask changes only after a ~2-dot
                # latency (blargg 10-even_odd_timing)
                was = rendering_enabled(ppu)
                now = value.bitwise_and(0x18) != 0
                if was != now {
                    { ppu: { ..ppu, mask: value, render_change_clock: ppu.clock, render_prev: was }, chr_write: NoChr, a12: KeepA12 }
                } else {
                    { ppu: { ..ppu, mask: value }, chr_write: NoChr, a12: KeepA12 }
                }
            }
            3 => { ppu: { ..ppu, oam_addr: value }, chr_write: NoChr, a12: KeepA12 }
            4 => {
                { ppu: { ..ppu,
                    oam: ppu.oam.set(ppu.oam_addr.to_u64(), value) ?? ppu.oam,
                    oam_addr: ppu.oam_addr.plus_wrap(1),
                }, chr_write: NoChr, a12: KeepA12 }
            }

            5 =>
                if ppu.latch == Bool.False {
                    # first write: coarse X + fine X
                    new_t = ppu.t.bitwise_and(0xFFE0).bitwise_or(value.shr_zf_wrap(3).to_u16())
                    { ppu: { ..ppu, t: new_t, fine_x: value.bitwise_and(0x07), latch: Bool.True }, chr_write: NoChr, a12: KeepA12 }
                } else {
                    # second write: coarse Y + fine Y
                    coarse_y = value.bitwise_and(0xF8).to_u16().shl_wrap(2)
                    fine_y = value.bitwise_and(0x07).to_u16().shl_wrap(12)
                    { ppu: { ..ppu, t: ppu.t.bitwise_and(0x8C1F).bitwise_or(coarse_y).bitwise_or(fine_y), latch: Bool.False }, chr_write: NoChr, a12: KeepA12 }
                }

            6 =>
                if ppu.latch == Bool.False {
                    new_t = ppu.t.bitwise_and(0x00FF).bitwise_or(value.bitwise_and(0x3F).to_u16().shl_wrap(8))
                    { ppu: { ..ppu, t: new_t, latch: Bool.True }, chr_write: NoChr, a12: KeepA12 }
                } else {
                    # second write loads v: the address bus follows it, and
                    # mid-line it repoints where the next span fetches from
                    # (raster splits)
                    new_t = ppu.t.bitwise_and(0xFF00).bitwise_or(value.to_u16())
                    { ppu: { ..ppu, t: new_t, v: new_t, span_v: new_t, latch: Bool.False }, chr_write: NoChr, a12: a12_of_v(ppu, new_t) }
                }

            7 => {
                addr = ppu.v.bitwise_and(0x3FFF)
                new_v = ppu.v.plus_wrap(vram_increment(ppu))
                advanced = { ..ppu, v: new_v, span_v: new_v }
                if addr < 0x2000 {
                    { ppu: advanced, chr_write: ChrAt(addr, value), a12: a12_of_v(ppu, new_v) }
                } else {
                    { ppu: ppu_write(advanced, addr, value), chr_write: NoChr, a12: a12_of_v(ppu, new_v) }
                }
            }

            _ => { ppu: ppu, chr_write: NoChr, a12: KeepA12 }
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

    # background pixels for a tile-aligned span: walk tiles from `from_v`
    # (the live fetch position), trim fine_x, emit `count` pixels; also
    # returns the fetch position advanced by the span's whole tiles so the
    # next span continues where this one stopped
    bg_span : Ppu, Cartridge, { l0 : U8, l1 : U8 }, U16, U64 -> { pixels : List(U8), v_after : U16, latches : { l0 : U8, l1 : U8 } }
    bg_span = |ppu, cart, lat0, from_v, count| {
        pattern_base : U16
        pattern_base = if ppu.ctrl.bitwise_and(0x10) != 0 { 0x1000 } else { 0x0000 }
        fine_y = from_v.shr_zf_wrap(12).bitwise_and(0x07)
        tiles_needed = count.plus(ppu.fine_x.to_u64()).plus(7).shr_zf_wrap(3).plus(1)
        walk = |vv, tile_i, acc, lat| {
            if tile_i >= tiles_needed {
                { row: acc, lat: lat }
            } else {
                nt_addr = vv.bitwise_and(0x0FFF).bitwise_or(0x2000)
                tile = ppu_read(ppu, cart, nt_addr)
                attr_addr = vv.bitwise_and(0x0C00).bitwise_or(0x23C0).bitwise_or(vv.shr_zf_wrap(4).bitwise_and(0x38)).bitwise_or(vv.shr_zf_wrap(2).bitwise_and(0x07))
                attr = ppu_read(ppu, cart, attr_addr)
                quad_shift = vv.shr_zf_wrap(4).bitwise_and(0x04).bitwise_or(vv.bitwise_and(0x02)).to_u8_wrap()
                pal2 = attr.shr_zf_wrap(quad_shift).bitwise_and(0x03)
                row_addr = pattern_base.plus_wrap(tile.to_u16().shl_wrap(4)).plus_wrap(fine_y)
                lo_f = cart.chr_fetch(lat, row_addr)
                hi_f = cart.chr_fetch(lo_f.latches, row_addr.plus_wrap(8))
                lo = lo_f.value
                hi = hi_f.value
                # pack pattern + palette into one byte: pal2*4 + pattern
                # (0 stays 0 = transparent); appended directly - no per-tile
                # list building (this runs 33x per scanline)
                pal_bits = pal2.shl_wrap(2)
                put = |a, bit| {
                    p0 = lo.shr_zf_wrap(bit).bitwise_and(1)
                    p1 = hi.shr_zf_wrap(bit).bitwise_and(1)
                    p = p1.shl_wrap(1).bitwise_or(p0) # 0-3 pattern value
                    a.append(if p == 0 { 0 } else { pal_bits.bitwise_or(p) })
                }
                acc2 = put(put(put(put(put(put(put(put(acc, 7), 6), 5), 4), 3), 2), 1), 0)
                # coarse X increment with nametable-x wrap
                next_v =
                    if vv.bitwise_and(0x001F) == 31 {
                        vv.bitwise_and(0xFFE0).bitwise_xor(0x0400)
                    } else {
                        vv.plus_wrap(1)
                    }
                walk(next_v, tile_i.plus(1), acc2, hi_f.latches)
            }
        }
        whole_tiles = count.shr_zf_wrap(3)
        walked = walk(from_v, 0, [], lat0)
        advance = |vv, k|
            if k == 0 {
                vv
            } else if vv.bitwise_and(0x001F) == 31 {
                advance(vv.bitwise_and(0xFFE0).bitwise_xor(0x0400), k - 1)
            } else {
                advance(vv.plus_wrap(1), k - 1)
            }
        {
            pixels: List.sublist(walked.row, { start: ppu.fine_x.to_u64(), len: count }),
            v_after: advance(from_v, whole_tiles),
            latches: walked.lat,
        }
    }

    # --- sprites ---

    sprite_height : Ppu -> U16
    sprite_height = |ppu| if ppu.ctrl.bitwise_and(0x20) != 0 { 16 } else { 8 }

    # per-pixel sprite row, one packed byte per pixel: 0 = transparent
    # (impossible for a real sprite pixel — bit 4 of the palette entry is
    # always set), bits 0-4 = palette entry (0x10 + pal*4 + pattern),
    # bit 5 = behind background, bit 6 = sprite 0
    sprite_row : Ppu, Cartridge, { l0 : U8, l1 : U8 }, U16 -> { row : List(U8), latches : { l0 : U8, l1 : U8 } }
    sprite_row = |ppu, cart, lat0, line| {
        height = sprite_height(ppu)
        scan = |idx, found, acc, lat| {
            if idx >= 64 or found >= 8 {
                { row: acc, latches: lat }
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
                    lo_f = cart.chr_fetch(lat, pattern_addr)
                    hi_f = cart.chr_fetch(lo_f.latches, pattern_addr.plus_wrap(8))
                    lo = lo_f.value
                    hi = hi_f.value
                    pal_bits = attr.bitwise_and(0x03).shl_wrap(2).bitwise_or(0x10)
                    flag_bits = attr.bitwise_and(0x20).bitwise_or(if idx == 0 { 0x40 } else { 0x00 })
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
                            } else if (acc2.get(x) ?? 0) != 0 {
                                # first (lowest OAM index) opaque sprite wins
                                place(acc2, i.plus(1))
                            } else {
                                px = pal_bits.bitwise_or(p).bitwise_or(flag_bits)
                                place(acc2.set(x, px) ?? acc2, i.plus(1))
                            }
                        }
                    }
                    scan(idx.plus_wrap(1), found.plus(1), place(acc, 0), hi_f.latches)
                } else {
                    scan(idx.plus_wrap(1), found, acc, lat)
                }
            }
        }
        empty : List(U8)
        empty = List.repeat(0, 256)
        idx0 : U16
        idx0 = 0
        found0 : U64
        found0 = 0
        scan(idx0, found0, empty, lat0)
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

    # render pixels [x0, x1) of `line` from CURRENT state (span model:
    # each span reflects the registers in effect when it renders, which is
    # what makes mid-line raster effects work). x0/x1 are tile-aligned
    # (x1 may be 256). The first span of a line seeds span_v from v; each
    # span advances it; $2006/$2007 v-loads overwrite it.
    render_span : Ppu, Cartridge, { l0 : U8, l1 : U8 }, U16, U64, U64 -> { ppu : Ppu, latches : { l0 : U8, l1 : U8 } }
    render_span = |ppu0, cart, lat0, line, x0, x1| {
        if x1 <= x0 {
            { ppu: ppu0, latches: lat0 }
        } else {
            ppu = if ppu0.rendered_x == 0 { { ..ppu0, span_v: ppu0.v } } else { ppu0 }
            backdrop = ppu.palette.get(0) ?? 0
            row_start = line.to_u64().shl_wrap(8) # line * 256
            count = x1.minus(x0)
            if rendering_enabled(ppu) == Bool.False {
                fill = |fb, x| if x >= x1 { fb } else { fill(fb.set(row_start.plus(x), backdrop.bitwise_and(0x3F)) ?? fb, x.plus(1)) }
                { ppu: { ..ppu, framebuffer: fill(ppu.framebuffer, x0), rendered_x: x1.to_u16_wrap() }, latches: lat0 }
            } else {
                show_bg = ppu.mask.bitwise_and(0x08) != 0
                show_sp = ppu.mask.bitwise_and(0x10) != 0
                show_bg_left = ppu.mask.bitwise_and(0x02) != 0
                show_sp_left = ppu.mask.bitwise_and(0x04) != 0
                bgr =
                    if show_bg {
                        bg_span(ppu, cart, lat0, ppu.span_v, count)
                    } else {
                        { pixels: List.repeat(0, count), v_after: ppu.span_v, latches: lat0 }
                    }
                # sprites fetch after the background (hardware order); latch
                # state flows BG -> sprites within the span
                spr = if show_sp { sprite_row(ppu, cart, bgr.latches, line) } else { { row: List.repeat(0, 256), latches: bgr.latches } }
                sp = spr.row
                merge = |st, x| {
                    if x >= x1 {
                        st
                    } else {
                        bg_entry = bgr.pixels.get(x.minus(x0)) ?? 0
                        bg_opaque0 = bg_entry.bitwise_and(0x03) != 0
                        bg_opaque = bg_opaque0 and (x >= 8 or show_bg_left)
                        sp_entry = if x >= 8 or show_sp_left { sp.get(x) ?? 0 } else { 0 }
                        merged =
                            if sp_entry == 0 {
                                { color: if bg_opaque { bg_entry } else { 0 }, hit: Bool.False }
                            } else {
                                s_color = sp_entry.bitwise_and(0x1F)
                                s_zero = sp_entry.bitwise_and(0x40) != 0
                                if bg_opaque and sp_entry.bitwise_and(0x20) != 0 {
                                    { color: bg_entry, hit: s_zero and x != 255 }
                                } else if bg_opaque {
                                    { color: s_color, hit: s_zero and x != 255 }
                                } else {
                                    { color: s_color, hit: Bool.False }
                                }
                            }
                        pal_addr = if merged.color == 0 { 0 } else { merged.color.to_u16().bitwise_or(0x3F00) }
                        color = ppu_read(ppu, cart, pal_addr.bitwise_or(0x3F00)).bitwise_and(0x3F)
                        fb2 = st.fb.set(row_start.plus(x), color) ?? st.fb
                        merge({ fb: fb2, hit: st.hit or merged.hit }, x.plus(1))
                    }
                }
                result = merge({ fb: ppu.framebuffer, hit: Bool.False }, x0)
                new_status =
                    if result.hit and show_bg and show_sp {
                        ppu.status.bitwise_or(0x40)
                    } else {
                        ppu.status
                    }
                { ppu: { ..ppu,
                    framebuffer: result.fb,
                    status: new_status,
                    rendered_x: x1.to_u16_wrap(),
                    span_v: bgr.v_after,
                }, latches: spr.latches }
            }
        }
    }

    # a register access at the synced dot on a visible line: commit the
    # pixels the pipeline has already produced (tile-granular, with a
    # two-tile fetch-ahead lead) before the access changes anything
    flush_to_access : Ppu, Cartridge -> { ppu : Ppu, latches : { l0 : U8, l1 : U8 } }
    flush_to_access = |ppu, cart| {
        lat = cart.chr_latches()
        if ppu.scanline < 240 and ppu.dot >= 1 {
            d = ppu.dot.to_u64()
            # pixel being output is d-1; the pipeline is ~2 tiles ahead
            x_split0 = d.minus(1).plus(16).plus(7).shr_zf_wrap(3).shl_wrap(3)
            x_split = if x_split0 > 256 { 256 } else { x_split0 }
            if x_split > ppu.rendered_x.to_u64() {
                render_span(ppu, cart, lat, ppu.scanline, ppu.rendered_x.to_u64(), x_split)
            } else {
                { ppu: ppu, latches: lat }
            }
        } else {
            { ppu: ppu, latches: lat }
        }
    }

    # line completed: render the remainder, then the end-of-line
    # bookkeeping (overflow flag, loopy v movement, span reset)
    finish_line : Ppu, Cartridge, { l0 : U8, l1 : U8 }, U16 -> { ppu : Ppu, latches : { l0 : U8, l1 : U8 } }
    finish_line = |ppu0, cart, lat0, line| {
        r = render_span(ppu0, cart, lat0, line, ppu0.rendered_x.to_u64(), 256)
        ppu = r.ppu
        # the loopy movement is anchored at dots 256/257, so judge rendering
        # by the state in effect THERE: a $2001 toggle later in the line
        # (hblank) must not retroactively add or remove this line's Y
        # increment. Battletoads enables rendering at dot ~290 of the
        # level-entry frame and its one-shot sprite-0 raster sync needs the
        # first line's increment skipped.
        at256 = ppu.clock.minus(85) # line end (dot 341) back to dot 256
        moving =
            if ppu.render_change_clock.plus(1) <= at256 {
                rendering_enabled(ppu)
            } else {
                ppu.render_prev
            }
        if moving {
            over = sprites_on_line(ppu, line) > 8
            with_over = if over { ppu.status.bitwise_or(0x20) } else { ppu.status }
            # loopy end-of-line movement: increment Y, copy horizontal from t
            new_v = copy_horizontal(increment_y(ppu.v), ppu.t)
            { ppu: { ..ppu, status: with_over, v: new_v, rendered_x: 0 }, latches: r.latches }
        } else {
            { ppu: { ..ppu, rendered_x: 0 }, latches: r.latches }
        }
    }

    # --- timing: absolute dot clock with lazy catch-up ---
    # The console does not tick the PPU per instruction: it materializes
    # exact dot state only when crossing a scheduled event (vblank set,
    # a mapper scanline clock) or at register access. Dot-1 events on
    # lines 241/261 (flag set/clear) are exact; rendering stays batched
    # per scanline, produced when a catch-up completes the line.

    # pre-render is one dot short on odd frames while rendering is enabled -
    # judged by the rendering state ~2 dots BEFORE dot 339, since $2001
    # writes take effect with a short latency
    line_len : Ppu -> U64
    line_len = |ppu|
        if ppu.scanline == 261 and ppu.odd_frame {
            t339 = ppu.clock.plus(339).minus(ppu.dot.to_u64())
            effective =
                if ppu.render_change_clock.plus(1) <= t339 {
                    rendering_enabled(ppu)
                } else {
                    ppu.render_prev
                }
            if effective { 340 } else { 341 }
        } else {
            341
        }

    # dots from the synced position to the next vblank set (241, dot 1)
    dots_until_vbl_set : Ppu -> U64
    dots_until_vbl_set = |ppu| {
        s = ppu.scanline.to_u64()
        d = ppu.dot.to_u64()
        if s < 241 or (s == 241 and d < 1) {
            (241 - s) * 341 + 1 - d
        } else {
            # rest of this frame (through the pre-render line), then 241
            # full lines and one dot of the next
            rest =
                if s == 261 {
                    line_len(ppu) - d
                } else {
                    (341 - d) + (260 - s) * 341 + line_len({ ..ppu, scanline: 261, dot: 0 })
                }
            rest + 241 * 341 + 1
        }
    }

    # A12's per-line waveform candidates (rise/fall dots), from the
    # pattern-table config. Sprites at $1000 (or 8x16) with BG at $0000:
    # one rise at the sprite fetches. BG at $1000 with sprites at $0000:
    # rises at the first tile fetch AND the prefetch - the mapper's
    # low-time filter picks the right one (dot ~8 after an idle span,
    # the previous line's prefetch in steady state, matching mmc3-4's
    # 256-dots-earlier and scanline-1 offsets). Matching tables never
    # toggle A12 (no IRQ clocks - real hardware). Dots calibrated by
    # blargg mmc3-4.
    a12_waveform : Ppu -> List({ rise : U64, fall : U64 })
    a12_waveform = |ppu| {
        sprites_high =
            if ppu.ctrl.bitwise_and(0x20) != 0 {
                Bool.True # 8x16: per-tile banks; approximated as sprite-high
            } else {
                ppu.ctrl.bitwise_and(0x08) != 0
            }
        bg_high = ppu.ctrl.bitwise_and(0x10) != 0
        if sprites_high and bg_high == Bool.False {
            [{ rise: 264, fall: 326 }]
        } else if bg_high and sprites_high == Bool.False {
            [{ rise: 8, fall: 257 }, { rise: 328, fall: 341 }]
        } else {
            []
        }
    }

    # the next clock at which the console MUST materialize the PPU: the
    # NMI edge at (241,1), plus - while an A12-clocked mapper (MMC3) is
    # rendering - the current or next qualifying line's rise dot. Flag
    # clears and rendering are lazy: register accesses sync first.
    next_event_after : Ppu, Bool -> U64
    next_event_after = |ppu, mapper_clocks| {
        vbl = ppu.clock.plus(dots_until_vbl_set(ppu))
        if mapper_clocks and rendering_enabled(ppu) {
            wave = a12_waveform(ppu)
            match wave.first() {
                Ok(first) => {
                    rd0 = first.rise
                    # earliest candidate still ahead in the current line
                    ahead = wave.fold(9999, |best, c| {
                        if c.rise >= ppu.dot.to_u64() and c.rise < best { c.rise } else { best }
                    })
                    s = ppu.scanline.to_u64()
                    d = ppu.dot.to_u64()
                    delta =
                        if (s < 240 or s == 261) and ahead < 9999 {
                            ahead.plus(1).minus(d)
                        } else if s < 239 {
                            (341 - d).plus(rd0.plus(1))
                        } else if s < 240 {
                            # 239 past its rises: next is the pre-render line
                            (341 - d).plus(21 * 341).plus(rd0.plus(1))
                        } else if s == 261 {
                            line_len(ppu).minus(d).plus(rd0.plus(1))
                        } else {
                            # vblank lines: the pre-render line's first rise
                            (261 - s) * 341 - d + rd0.plus(1)
                        }
                    ev = ppu.clock.plus(delta)
                    if ev < vbl { ev } else { vbl }
                }

                Err(_) => vbl
            }
        } else {
            vbl
        }
    }

    # advance the synced position to `target` (absolute dots), applying
    # dot-1 events exactly and rendering completed scanlines. When
    # want_a12 is set (MMC3 carts), each qualifying line's A12 rise is
    # stamped with its exact clock into the returned pulse list.
    catch_up : Ppu, Cartridge, U64, Bool -> { ppu : Ppu, a12 : List({ at : U64, fall : U64 }), latches : { l0 : U8, l1 : U8 } }
    catch_up = |ppu0, cart, target, want_a12| catch_go(ppu0, cart, target, want_a12, [], cart.chr_latches())

    catch_go : Ppu, Cartridge, U64, Bool, List({ at : U64, fall : U64 }), { l0 : U8, l1 : U8 } -> { ppu : Ppu, a12 : List({ at : U64, fall : U64 }), latches : { l0 : U8, l1 : U8 } }
    catch_go = |ppu0, cart, target, want_a12, acc, lat| {
        if ppu0.clock >= target {
            { ppu: ppu0, a12: acc, latches: lat }
        } else {
            ll = line_len(ppu0)
            d = ppu0.dot.to_u64()
            line_end = ppu0.clock.plus(ll.minus(d))
            # dot-1 flag events on lines 241 (set) and 261 (clear)
            event_stop =
                if (ppu0.scanline == 241 or ppu0.scanline == 261) and d < 1 {
                    ppu0.clock.plus(1)
                } else {
                    line_end
                }
            stop0 = if event_stop < line_end { event_stop } else { line_end }
            stop = if target < stop0 { target } else { stop0 }
            span = stop.minus(ppu0.clock)
            # stamp the line's A12 rises this advance crosses (the mapper's
            # filter decides which ones count)
            acc2 =
                if want_a12 and rendering_enabled(ppu0) and (ppu0.scanline < 240 or ppu0.scanline == 261) {
                    a12_waveform(ppu0).fold(acc, |a, c| {
                        if d <= c.rise and d.plus(span) > c.rise {
                            at = ppu0.clock.plus(c.rise.minus(d))
                            a.append({ at: at, fall: at.plus(c.fall.minus(c.rise)) })
                        } else {
                            a
                        }
                    })
                } else {
                    acc
                }
            advanced = { ..ppu0, clock: stop, dot: ppu0.dot.plus_wrap(span.to_u16_wrap()) }
            with_event =
                if advanced.dot == 1 and d < 1 and advanced.scanline == 241 {
                    # vblank entry: the frame counter always advances; the
                    # flag and NMI are skipped when a $2002 read one dot
                    # earlier won the race
                    based = { ..advanced, frame: advanced.frame.plus(1) }
                    if based.vbl_suppress {
                        { ..based, vbl_suppress: Bool.False }
                    } else {
                        set = { ..based,
                            status: based.status.bitwise_or(0x80),
                            vbl_set_clock: based.clock,
                        }
                        if set.ctrl.bitwise_and(0x80) != 0 {
                            { ..set, nmi_pending: Bool.True }
                        } else {
                            set
                        }
                    }
                } else if advanced.dot == 1 and d < 1 and advanced.scanline == 261 {
                    { ..advanced, status: advanced.status.bitwise_and(0x1F) }
                } else {
                    advanced
                }
            if with_event.dot.to_u64() >= ll {
                # line completed: render it, then move to the next
                line = with_event.scanline
                rendered =
                    if line < 240 {
                        finish_line(with_event, cart, lat, line)
                    } else {
                        { ppu: with_event, latches: lat }
                    }
                next =
                    if line == 261 {
                        wrapped = { ..rendered.ppu,
                            dot: 0,
                            scanline: 0,
                            odd_frame: if rendered.ppu.odd_frame { Bool.False } else { Bool.True },
                        }
                        if rendering_enabled(wrapped) {
                            { ..wrapped, v: copy_vertical(wrapped.v, wrapped.t) }
                        } else {
                            wrapped
                        }
                    } else {
                        { ..rendered.ppu, dot: 0, scanline: line.plus_wrap(1) }
                    }
                catch_go(next, cart, target, want_a12, acc2, rendered.latches)
            } else {
                catch_go(with_event, cart, target, want_a12, acc2, lat)
            }
        }
    }

    # legacy relative advance (expects and harnesses)
    tick : Ppu, Cartridge, U64 -> { ppu : Ppu, a12 : List({ at : U64, fall : U64 }), latches : { l0 : U8, l1 : U8 } }
    tick = |ppu0, cart, dots| catch_up(ppu0, cart, ppu0.clock.plus(dots), Bool.True)

    # DMA landing: 256 bytes written through OAMADDR, wrapping.
    # NOTE: written as an explicit recursive loop, not a fold with an index
    # counter in the accumulator record - the roc nightly's build backend
    # (2026-08-07) miscompiles that shape (the counter increments in place
    # before the body reads it, shifting every write by one; the interpreter
    # is correct). Minimal repro: check/inspect/repro.roc.
    load_oam : Ppu, List(U8) -> Ppu
    load_oam = |ppu, data| {
        write_all = |oam, k|
            if k >= data.len() {
                oam
            } else {
                dst = ppu.oam_addr.to_u64().plus(k).bitwise_and(0xFF)
                write_all(oam.set(dst, data.get(k) ?? 0) ?? oam, k.plus(1))
            }
        z : U64
        z = 0
        { ..ppu, oam: write_all(ppu.oam, z) }
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

# mid-scanline raster split: repointing v via $2006 mid-line renders the
# left of the line from the old scroll and the right from the new one,
# split at a tile boundary just ahead of the write
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 0, 0x01, 0x00, 0, 0, 0, 0, 0, 0, 0, 0] # CHR RAM, vertical
    rom = header.concat(List.repeat(0xEA, 16384))
    match Cartridge.from_bytes(rom) {
        Ok(cart0) => {
            # CHR tile 1: solid pattern-1 pixels (lo plane 0xFF each row)
            fill_tile : Cartridge, U16 -> Cartridge
            fill_tile = |c, i| if i >= 8 { c } else { fill_tile(c.write_chr(i.plus(0x0010), 0xFF), i.plus(1)) }
            cart = fill_tile(cart0, 0)
            p0 = Ppu.init(Vertical)
            # nametable 1 (0x400+) filled with tile 1; nametable 0 stays tile 0
            fill_nt : List(U8), U64 -> List(U8)
            fill_nt = |vr, i| if i >= 0x3C0 { vr } else { fill_nt(vr.set(i.plus(0x400), 1) ?? vr, i.plus(1)) }
            i0 : U64
            i0 = 0
            seeded = { ..p0,
                mask: 0x0A, # BG on, left column shown
                vram: fill_nt(p0.vram, i0),
                palette: (p0.palette.set(0, 0x0F) ?? p0.palette).set(1, 0x21) ?? p0.palette,
            }
            # into line 5, dot 100
            mid = seeded.catch_up(cart, 5 * 341 + 100, Bool.False).ppu
            flushed = mid.flush_to_access(cart).ppu
            w1 = flushed.write_reg(6, 0x04).ppu # v high: nametable 1
            w2 = w1.write_reg(6, 0x00).ppu # v low: coarse 0
            done = w2.catch_up(cart, 6 * 341, Bool.False).ppu
            row : U64
            row = 5 * 256
            left = done.framebuffer.get(row.plus(50)) ?? 0xFF
            split_edge = done.framebuffer.get(row.plus(119)) ?? 0xFF
            right = done.framebuffer.get(row.plus(200)) ?? 0xFF
            # split at ceil((99+16)/8)*8 = 120: backdrop left, tile-1 right
            left == 0x0F and split_edge == 0x0F and right == 0x21
        }

        Err(_) => Bool.False
    }
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
