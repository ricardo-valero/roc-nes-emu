import /Memory
import /Cartridge

# The CPU's single read/write path.
# Flat: 64 KiB, for state-injection harnesses (SingleStepTests) and expects.
# Nrom: the NES CPU memory map with a mapper-0 cartridge:
#   0x0000-0x1FFF  2 KiB internal RAM, mirrored every 0x0800
#   0x2000-0x3FFF  PPU registers - stubbed until the PPU capability
#   0x4000-0x401F  APU / IO - stubbed
#   0x4020-0x7FFF  unmapped (no PRG RAM on NROM in scope)
#   0x8000-0xFFFF  cartridge PRG (writes ignored)
Bus := [
    Flat(List(U8)),
    Nrom({ ram : List(U8), cart : Cartridge }),
].{
    flat : List(U8) -> Bus
    flat = |mem| Flat(mem)

    from_cartridge : Cartridge -> Bus
    from_cartridge = |cart| Nrom({ ram: List.repeat(0, 0x0800), cart: cart })

    read8 : Bus, U16 -> U8
    read8 = |bus, addr|
        match bus {
            Flat(mem) => Memory.read8(mem, addr)
            Nrom(n) =>
                if addr < 0x2000 {
                    n.ram.get(addr.bitwise_and(0x07FF).to_u64()) ?? 0
                } else if addr < 0x8000 {
                    0 # stub regions; refined by the PPU/APU capabilities
                } else {
                    n.cart.read_prg(addr)
                }
        }

    write8 : Bus, U16, U8 -> Bus
    write8 = |bus, addr, v|
        match bus {
            Flat(mem) => Flat(Memory.write8(mem, addr, v))
            Nrom(n) =>
                if addr < 0x2000 {
                    Nrom({ ..n, ram: n.ram.set(addr.bitwise_and(0x07FF).to_u64(), v) ?? n.ram })
                } else {
                    bus # stubs and PRG ignore writes
                }
        }

    read16 : Bus, U16 -> U16
    read16 = |bus, pos| {
        lo = read8(bus, pos)
        hi = read8(bus, pos.plus(1))
        hi.to_u16().shl_wrap(8).bitwise_or(lo.to_u16())
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
}

# RAM mirroring on the NES map
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 0, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    rom = header.concat(List.repeat(0xEA, 16384))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            bus = Bus.from_cartridge(cart).write8(0x0005, 0x42)
            bus.read8(0x0805) == 0x42
            and bus.read8(0x1005) == 0x42
            and bus.read8(0x1805) == 0x42
            and bus.read8(0x2002) == 0 # PPU stub
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
            bus.read16(0xFFFC) == 0xC000
        }

        Err(_) => Bool.False
    }
}
