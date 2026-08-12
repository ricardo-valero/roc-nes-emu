import /Memory
import /Cartridge
import /Ppu

# The CPU's single read/write path.
# Flat: 64 KiB, for state-injection harnesses (SingleStepTests) and expects.
# Nrom: the NES CPU memory map with a mapper-0 cartridge:
#   0x0000-0x1FFF  2 KiB internal RAM, mirrored every 0x0800
#   0x2000-0x3FFF  PPU registers, mirrored every 8 bytes (reads have effects)
#   0x4014         OAM DMA (write): instant page copy + 513-cycle CPU stall
#   0x4000-0x401F  other APU / IO - stubbed
#   0x6000-0x7FFF  8 KiB PRG RAM (blargg test ROMs report status here)
#   0x8000-0xFFFF  cartridge PRG (writes ignored)
Bus := [
    Flat(List(U8)),
    Nrom({ ram : List(U8), cart : Cartridge, prg_ram : List(U8), ppu : Ppu, dma_stall : U64 }),
].{
    flat : List(U8) -> Bus
    flat = |mem| Flat(mem)

    from_cartridge : Cartridge -> Bus
    from_cartridge = |cart|
        Nrom({
            ram: List.repeat(0, 0x0800),
            cart: cart,
            prg_ram: List.repeat(0, 0x2000),
            ppu: Ppu.init(cart.header.mirroring),
            dma_stall: 0,
        })

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
                } else if addr < 0x6000 {
                    { bus: bus, value: 0 } # APU/IO stubs
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
                    Nrom({ ..n, ppu: n.ppu.write_reg(addr.bitwise_and(0x0007), v) })
                } else if addr == 0x4014 {
                    oam_dma(bus, v)
                } else if addr >= 0x6000 and addr < 0x8000 {
                    Nrom({ ..n, prg_ram: n.prg_ram.set(addr.bitwise_and(0x1FFF).to_u64(), v) ?? n.prg_ram })
                } else {
                    bus # stubs and PRG ignore writes
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

    # advance the PPU by `dots`; reports whether an NMI should fire
    tick_ppu : Bus, U64 -> { bus : Bus, value : Bool }
    tick_ppu = |bus, dots|
        match bus {
            Flat(_) => { bus: bus, value: Bool.False }
            Nrom(n) => {
                ticked = n.ppu.tick(n.cart, dots)
                r = ticked.take_nmi()
                { bus: Nrom({ ..n, ppu: r.ppu }), value: r.value }
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
