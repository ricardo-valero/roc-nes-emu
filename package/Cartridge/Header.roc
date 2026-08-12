# iNES / NES 2.0 header (16 bytes)
# https://www.nesdev.org/wiki/INES
# https://www.nesdev.org/wiki/NES_2.0
Header := {
    prg_rom_size : U64, # bytes
    chr_rom_size : U64, # bytes
    mapper : U16,
    mirroring : [Horizontal, Vertical, FourScreen],
    battery : Bool,
    trainer : Bool,
    format : [INes, Nes2],
}.{
    parse : List(U8) -> Try(Header, [NotANesFile])
    parse = |bytes| {
        b0 = bytes.get(0) ?? 0
        b1 = bytes.get(1) ?? 0
        b2 = bytes.get(2) ?? 0
        b3 = bytes.get(3) ?? 0
        if bytes.len() < 16 or b0 != 0x4E or b1 != 0x45 or b2 != 0x53 or b3 != 0x1A {
            # "NES\x1A"
            Err(NotANesFile)
        } else {
            prg_units = bytes.get(4) ?? 0
            chr_units = bytes.get(5) ?? 0
            flags6 = bytes.get(6) ?? 0
            flags7 = bytes.get(7) ?? 0
            header : Header
            header = {
                prg_rom_size: prg_units.to_u64().shl_wrap(14), # x 16 KiB
                chr_rom_size: chr_units.to_u64().shl_wrap(13), # x 8 KiB
                mapper: flags6.shr_zf_wrap(4).bitwise_or(flags7.bitwise_and(0xF0)).to_u16(),
                mirroring:
                if flags6.bitwise_and(0x08) != 0 {
                    FourScreen
                } else if flags6.bitwise_and(0x01) != 0 {
                    Vertical
                } else {
                    Horizontal
                },
                battery: flags6.bitwise_and(0x02) != 0,
                trainer: flags6.bitwise_and(0x04) != 0,
                format: if flags7.bitwise_and(0x0C) == 0x08 { Nes2 } else { INes },
            }
            Ok(header)
        }
    }
}

# 16 KiB PRG + 8 KiB CHR, mapper 0, vertical mirroring (nestest's shape)
expect {
    h = Header.parse([0x4E, 0x45, 0x53, 0x1A, 1, 1, 0x01, 0x00, 0, 0, 0, 0, 0, 0, 0, 0])
    match h {
        Ok(header) =>
            header.prg_rom_size == 16384
            and header.chr_rom_size == 8192
            and header.mapper == 0
            and header.mirroring == Vertical
            and header.trainer == Bool.False
            and header.format == INes

        Err(_) => Bool.False
    }
}

# mapper nibbles combine; NES 2.0 detected via flags7 bits 2-3 == 0b10
expect {
    h = Header.parse([0x4E, 0x45, 0x53, 0x1A, 2, 0, 0x40, 0x18, 0, 0, 0, 0, 0, 0, 0, 0])
    match h {
        Ok(header) => header.mapper == 0x14 and header.format == Nes2 and header.prg_rom_size == 32768
        Err(_) => Bool.False
    }
}

# rejection path
expect {
    match Header.parse([1, 2, 3]) {
        Err(NotANesFile) => Bool.True
        _ => Bool.False
    }
}
expect {
    match Header.parse([0x4E, 0x45, 0x53, 0x00, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]) {
        Err(NotANesFile) => Bool.True
        _ => Bool.False
    }
}
