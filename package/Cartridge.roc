import /Cartridge/Header

# A parsed ROM file: header + PRG/CHR data, with mapper-0 (NROM) PRG access.
# Other mappers arrive as their own change; until then `read_prg` assumes NROM.
Cartridge := {
    header : Header,
    prg : List(U8),
    chr : List(U8),
}.{
    from_bytes : List(U8) -> Try(Cartridge, [NotANesFile])
    from_bytes = |bytes| {
        header = Header.parse(bytes)?
        trainer_len : U64
        trainer_len = if header.trainer { 512 } else { 0 }
        prg_start = trainer_len.plus(16)
        cart : Cartridge
        cart = {
            header: header,
            prg: List.sublist(bytes, { start: prg_start, len: header.prg_rom_size }),
            chr: List.sublist(bytes, { start: prg_start.plus(header.prg_rom_size), len: header.chr_rom_size }),
        }
        Ok(cart)
    }

    # CPU reads in 0x8000-0xFFFF; 16 KiB PRG mirrors across both halves
    read_prg : Cartridge, U16 -> U8
    read_prg = |cart, addr| {
        offset = addr.bitwise_and(0x7FFF).to_u64()
        masked = if cart.header.prg_rom_size <= 16384 { offset.bitwise_and(0x3FFF) } else { offset }
        cart.prg.get(masked) ?? 0
    }
}

# synthetic 16 KiB cartridge: mirroring and the reset vector path
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 0, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    prg = List.repeat(0xEA, 16384).set(0x0005, 0x42) ?? []
    with_vec = prg.set(0x3FFC, 0x00) ?? []
    rom = header.concat(with_vec.set(0x3FFD, 0xC0) ?? [])
    match Cartridge.from_bytes(rom) {
        Ok(cart) =>
            cart.read_prg(0x8005) == 0x42
            and cart.read_prg(0xC005) == 0x42 # 16 KiB mirror
            and cart.read_prg(0xFFFC) == 0x00
            and cart.read_prg(0xFFFD) == 0xC0

        Err(_) => Bool.False
    }
}

# 32 KiB PRG maps directly (no mirroring)
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 2, 0, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    prg = List.repeat(0x00, 32768).set(0x4005, 0x99) ?? []
    rom = header.concat(prg)
    match Cartridge.from_bytes(rom) {
        Ok(cart) => cart.read_prg(0xC005) == 0x99 and cart.read_prg(0x8005) == 0x00
        Err(_) => Bool.False
    }
}
