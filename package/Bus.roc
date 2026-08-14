import /Memory
import /Cartridge
import /Ppu
import /Apu

# The CPU's single read/write path.
# Flat: 64 KiB, for state-injection harnesses (SingleStepTests) and expects.
# Nrom: the NES CPU memory map with a mapper-0 cartridge:
#   0x0000-0x1FFF  2 KiB internal RAM, mirrored every 0x0800
#   0x2000-0x3FFF  PPU registers, mirrored every 8 bytes (reads have effects)
#   0x4014         OAM DMA (write): instant page copy + 513-cycle CPU stall
#   0x4016         controller 1: strobe write, serial shift-register read
#   0x4000-0x4013, 0x4015, 0x4017  live APU registers ($4015 reads ack IRQ)
#   remaining 0x4000-0x401F  open-bus stubs
#   0x6000-0x7FFF  8 KiB PRG RAM (blargg test ROMs report status here)
#   0x8000-0xFFFF  cartridge PRG (writes ignored)
Bus := [
    Flat(List(U8)),
    Nrom({
        ram : List(U8),
        cart : Cartridge,
        prg_ram : List(U8),
        ppu : Ppu,
        apu : Apu,
        dma_stall : U64,
        buttons : { a : Bool, b : Bool, select : Bool, start : Bool, up : Bool, down : Bool, left : Bool, right : Bool },
        strobe : Bool,
        shift : U8,
    }),
].{
    flat : List(U8) -> Bus
    flat = |mem| Flat(mem)

    from_cartridge : Cartridge -> Bus
    from_cartridge = |cart|
        Nrom({
            ram: List.repeat(0, 0x0800),
            cart: cart,
            prg_ram: List.repeat(0, 0x2000),
            ppu: Ppu.init(Cartridge.current_mirroring(cart)),
            apu: Apu.init({}),
            dma_stall: 0,
            buttons: { a: Bool.False, b: Bool.False, select: Bool.False, start: Bool.False, up: Bool.False, down: Bool.False, left: Bool.False, right: Bool.False },
            strobe: Bool.False,
            shift: 0xFF,
        })

    # pack buttons into the hardware latch order: A first (bit 0) .. Right (bit 7)
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

    set_buttons : Bus, { a : Bool, b : Bool, select : Bool, start : Bool, up : Bool, down : Bool, left : Bool, right : Bool } -> Bus
    set_buttons = |bus, buttons|
        match bus {
            Flat(_) => bus
            Nrom(n) => Nrom({ ..n, buttons: buttons })
        }

    # Reads are state-returning: PPU registers have read side effects
    # (PPUSTATUS clears vblank + the write latch; PPUDATA cycles its buffer).
    # Side-effect-free regions return the bus unchanged.
    read8 : Bus, U16 -> { bus : Bus, value : U8 }
    read8 = |bus, addr|
        match bus {
            Flat(mem) => { bus: bus, value: Memory.read8(mem, addr) }
            Nrom(n) =>
                if addr < 0x2000 {
                    { bus: bus, value: n.ram.get(addr.bitwise_and(0x07FF).to_u64()) ?? 0 }
                } else if addr < 0x4000 {
                    r = Ppu.read_reg(n.ppu, n.cart, addr.bitwise_and(0x0007))
                    { bus: Nrom({ ..n, ppu: r.ppu }), value: r.value }
                } else if addr == 0x4016 {
                    if n.strobe {
                        # strobe held: live A, no shifting
                        { bus: bus, value: pack_buttons(n.buttons).bitwise_and(0x01) }
                    } else {
                        bit = n.shift.bitwise_and(0x01)
                        { bus: Nrom({ ..n, shift: n.shift.shr_zf_wrap(1).bitwise_or(0x80) }), value: bit }
                    }
                } else if addr == 0x4015 {
                    r = n.apu.read_status()
                    { bus: Nrom({ ..n, apu: r.apu }), value: r.value }
                } else if addr < 0x6000 {
                    { bus: bus, value: 0 } # remaining stubs ($4017: no second controller)
                } else if addr < 0x8000 {
                    { bus: bus, value: n.prg_ram.get(addr.bitwise_and(0x1FFF).to_u64()) ?? 0 }
                } else {
                    { bus: bus, value: n.cart.read_prg(addr) }
                }
        }

    write8 : Bus, U16, U8 -> Bus
    write8 = |bus, addr, v|
        match bus {
            Flat(mem) => Flat(Memory.write8(mem, addr, v))
            Nrom(n) =>
                if addr < 0x2000 {
                    Nrom({ ..n, ram: n.ram.set(addr.bitwise_and(0x07FF).to_u64(), v) ?? n.ram })
                } else if addr < 0x4000 {
                    r = n.ppu.write_reg(addr.bitwise_and(0x0007), v)
                    cart2 =
                        match r.chr_write {
                            ChrAt(chr_addr, chr_val) => n.cart.write_chr(chr_addr, chr_val)
                            NoChr => n.cart
                        }
                    Nrom({ ..n, ppu: r.ppu, cart: cart2 })
                } else if addr == 0x4014 {
                    oam_dma(bus, v)
                } else if addr == 0x4016 {
                    if v.bitwise_and(0x01) != 0 {
                        Nrom({ ..n, strobe: Bool.True })
                    } else {
                        # falling edge latches the current buttons
                        Nrom({ ..n, strobe: Bool.False, shift: pack_buttons(n.buttons) })
                    }
                } else if addr >= 0x4000 and addr <= 0x4017 {
                    # $4014/$4016 matched above; the rest is the APU
                    Nrom({ ..n, apu: n.apu.write_reg(addr, v) })
                } else if addr >= 0x6000 and addr < 0x8000 {
                    Nrom({ ..n, prg_ram: n.prg_ram.set(addr.bitwise_and(0x1FFF).to_u64(), v) ?? n.prg_ram })
                } else if addr >= 0x8000 {
                    # mapper registers (bank switching, mirroring, IRQ control)
                    cart2 = n.cart.write_prg(addr, v)
                    Nrom({ ..n, cart: cart2, ppu: n.ppu.set_mirroring(Cartridge.current_mirroring(cart2)) })
                } else {
                    bus # remaining stubs ignore writes
                }
        }

    # $4014: copy page v<<8..v<<8+0xFF into PPU OAM (instant) and record the
    # 513-cycle CPU stall for the console layer to fold in
    oam_dma : Bus, U8 -> Bus
    oam_dma = |bus0, page| {
        base = page.to_u16().shl_wrap(8)
        copy = |st, i| {
            if i > 255 {
                st
            } else {
                r = read8(st.bus, base.plus_wrap(i))
                copy({ bus: r.bus, data: st.data.append(r.value) }, i.plus(1))
            }
        }
        start : { bus : Bus, data : List(U8) }
        start = { bus: bus0, data: [] }
        result = copy(start, 0)
        match result.bus {
            Nrom(n) => Nrom({ ..n, ppu: n.ppu.load_oam(result.data), dma_stall: n.dma_stall.plus(513) })
            other => other
        }
    }

    read16 : Bus, U16 -> { bus : Bus, value : U16 }
    read16 = |bus0, pos| {
        lo = read8(bus0, pos)
        hi = read8(lo.bus, pos.plus(1))
        { bus: hi.bus, value: hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16()) }
    }

    write16 : Bus, U16, U16 -> Bus
    write16 = |bus, pos, data| {
        hi = data.shr_zf_wrap(8).to_u8_wrap()
        lo = data.bitwise_and(0xFF).to_u8_wrap()
        write8(write8(bus, pos, lo), pos.plus(1), hi)
    }

    # load a program image at 0x8000 with the reset vector pointing at it
    # (Flat-only; behavioral test helper)
    load_flat : Bus, List(U8) -> Bus
    load_flat = |bus, program|
        match bus {
            Flat(mem) => Flat(Memory.load(mem, program))
            other => other
        }

    # --- console-layer helpers ---

    # advance the PPU by `dots`; clocks the mapper's scanline IRQ counter and
    # reports the NMI latch and the mapper IRQ level
    tick_ppu : Bus, U64 -> { bus : Bus, nmi : Bool, irq : Bool }
    tick_ppu = |bus, dots|
        match bus {
            Flat(_) => { bus: bus, nmi: Bool.False, irq: Bool.False }
            Nrom(n) => {
                t = n.ppu.tick(n.cart, dots)
                r = t.ppu.take_nmi()
                clock_all = |st, k|
                    if k == 0 {
                        st
                    } else {
                        c = st.cart.clock_scanline()
                        clock_all({ cart: c.cart, irq: st.irq or c.irq }, k - 1)
                    }
                clocked = clock_all({ cart: n.cart, irq: Bool.False }, t.sl_clocks)
                { bus: Nrom({ ..n, ppu: r.ppu, cart: clocked.cart }), nmi: r.value, irq: clocked.irq }
            }
        }

    # advance the APU by CPU cycles; DMC fetch stalls fold into the DMA
    # stall account, and the level of the frame/DMC IRQ lines is reported
    tick_apu : Bus, U64 -> { bus : Bus, irq : Bool }
    tick_apu = |bus, cycles|
        match bus {
            Flat(_) => { bus: bus, irq: Bool.False }
            Nrom(n) => {
                r = n.apu.tick(n.cart, cycles)
                { bus: Nrom({ ..n, apu: r.apu, dma_stall: n.dma_stall.plus(r.stall) }), irq: r.irq }
            }
        }

    take_samples : Bus -> { bus : Bus, samples : List(F32) }
    take_samples = |bus|
        match bus {
            Flat(_) => { bus: bus, samples: [] }
            Nrom(n) => {
                r = n.apu.take_samples()
                { bus: Nrom({ ..n, apu: r.apu }), samples: r.samples }
            }
        }

    # consume an NMI latched during instruction execution (a $2000 write
    # enabling NMI during vblank) - delivered one instruction late by the console
    take_nmi : Bus, {} -> { bus : Bus, value : Bool }
    take_nmi = |bus, _|
        match bus {
            Flat(_) => { bus: bus, value: Bool.False }
            Nrom(n) => {
                r = n.ppu.take_nmi()
                { bus: Nrom({ ..n, ppu: r.ppu }), value: r.value }
            }
        }

    # consume any pending DMA stall cycles
    take_stall : Bus, {} -> { bus : Bus, value : U64 }
    take_stall = |bus, _|
        match bus {
            Flat(_) => { bus: bus, value: 0 }
            Nrom(n) =>
                if n.dma_stall == 0 {
                    { bus: bus, value: 0 }
                } else {
                    { bus: Nrom({ ..n, dma_stall: 0 }), value: n.dma_stall }
                }
        }

    ppu_frame : Bus -> U64
    ppu_frame = |bus|
        match bus {
            Flat(_) => 0
            Nrom(n) => n.ppu.frame
        }

    ppu_framebuffer : Bus -> List(U8)
    ppu_framebuffer = |bus|
        match bus {
            Flat(_) => []
            Nrom(n) => n.ppu.framebuffer
        }
}

# RAM mirroring on the NES map
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 0, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    rom = header.concat(List.repeat(0xEA, 16384))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            bus = Bus.from_cartridge(cart).write8(0x0005, 0x42)
            bus.read8(0x0805).value == 0x42
            and bus.read8(0x1005).value == 0x42
            and bus.read8(0x1805).value == 0x42
        }

        Err(_) => Bool.False
    }
}

# vectors come from cartridge PRG; PRG writes are ignored
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 0, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    prg0 = List.repeat(0xEA, 16384).set(0x3FFC, 0x00) ?? []
    prg = prg0.set(0x3FFD, 0xC0) ?? []
    rom = header.concat(prg)
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            bus = Bus.from_cartridge(cart).write8(0xFFFC, 0x77)
            bus.read16(0xFFFC).value == 0xC000
        }

        Err(_) => Bool.False
    }
}

# PRG RAM round-trips at 0x6000 (blargg status protocol depends on it)
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 0, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    rom = header.concat(List.repeat(0xEA, 16384))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            bus = Bus.from_cartridge(cart).write8(0x6000, 0x80).write8(0x7FFF, 0x33)
            bus.read8(0x6000).value == 0x80 and bus.read8(0x7FFF).value == 0x33
        }

        Err(_) => Bool.False
    }
}

# PPU register mirroring with read effects: reading 0x3FFA (mirror of 0x2002)
# returns and clears vblank on the returned bus
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 1, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    rom = header.concat(List.repeat(0xEA, 16384)).concat(List.repeat(0, 8192))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            bus0 = Bus.from_cartridge(cart)
            # reach vblank by ticking the PPU there (241 lines + 1 dot)
            t = bus0.tick_ppu(82182)
            r1 = t.bus.read8(0x3FFA)
            r2 = r1.bus.read8(0x2002)
            r1.value.bitwise_and(0x80) != 0 and r2.value.bitwise_and(0x80) == 0
        }

        Err(_) => Bool.False
    }
}

# OAM DMA copies a page from RAM and records the stall
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 1, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    rom = header.concat(List.repeat(0xEA, 16384)).concat(List.repeat(0, 8192))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            seeded = Bus.from_cartridge(cart).write8(0x0200, 0x11).write8(0x02FF, 0x99)
            dma = seeded.write8(0x4014, 0x02)
            st = dma.take_stall({})
            oam0 = st.bus.read8(0x2004).value # OAMADDR is 0 -> first byte
            oam0 == 0x11 and st.value == 513
        }

        Err(_) => Bool.False
    }
}

# controller: serial read order A,B,Select,Start,Up,Down,Left,Right; 1s past the eighth
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 1, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    rom = header.concat(List.repeat(0xEA, 16384)).concat(List.repeat(0, 8192))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            held = { a: Bool.False, b: Bool.False, select: Bool.False, start: Bool.True, up: Bool.False, down: Bool.False, left: Bool.False, right: Bool.True }
            latched = Bus.from_cartridge(cart).set_buttons(held).write8(0x4016, 1).write8(0x4016, 0)
            read_bits = |st, k| {
                if k >= 9 {
                    st
                } else {
                    r = st.bus.read8(0x4016)
                    read_bits({ bus: r.bus, bits: st.bits.append(r.value.bitwise_and(0x01)) }, k.plus(1))
                }
            }
            k0 : U64
            k0 = 0
            result = read_bits({ bus: latched, bits: [] }, k0)
            result.bits == [0, 0, 0, 1, 0, 0, 0, 1, 1]
        }

        Err(_) => Bool.False
    }
}

# strobe held high: every read reflects live A without shifting
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 1, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    rom = header.concat(List.repeat(0xEA, 16384)).concat(List.repeat(0, 8192))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            held = { a: Bool.True, b: Bool.False, select: Bool.False, start: Bool.False, up: Bool.False, down: Bool.False, left: Bool.False, right: Bool.False }
            strobed = Bus.from_cartridge(cart).set_buttons(held).write8(0x4016, 1)
            r1 = strobed.read8(0x4016)
            r2 = r1.bus.read8(0x4016)
            r1.value.bitwise_and(1) == 1 and r2.value.bitwise_and(1) == 1
        }

        Err(_) => Bool.False
    }
}
