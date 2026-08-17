import /Cartridge/Header

# A parsed ROM file: header + PRG/CHR data + mapper state.
# Mapper dispatch covers the classic set (~90% of licensed titles):
#   0 NROM        - no registers
#   1 MMC1        - serial shift register: PRG modes, 4K/8K CHR, mirroring
#   2 UxROM       - switchable 16K PRG at 0x8000, last bank fixed
#   3 CNROM       - switchable 8K CHR
#   4 MMC3        - 8K PRG modes, 2K+1K CHR windows, mirroring, scanline IRQ
#   7 AxROM       - switchable 32K PRG, single-screen mirroring select
#   9 MMC2        - 8K PRG + last-3 fixed, fetch-driven 4K CHR latches, mirroring
#  11 ColorDreams - switchable 32K PRG + 8K CHR, bus conflicts
#  66 GxROM       - switchable 32K PRG + 8K CHR, bus conflicts
# Unknown mappers parse but are marked Unsupported (NROM-fallback reads).
# Bank counts are assumed power-of-two (true of the licensed library).
Cartridge := {
    header : Header,
    hash : U64, # FNV-1a over the full ROM file bytes (snapshot identity)
    prg : List(U8),
    chr : List(U8), # CHR ROM, or 8K CHR RAM when the header reports none
    chr_writable : Bool,
    mapper : [
        Nrom,
        Uxrom({ bank : U8 }),
        Cnrom({ bank : U8 }),
        Axrom({ bank : U8 }), # latched register byte: bits 0-2 PRG, bit 4 nametable
        ColorDreams({ bank : U8 }), # latched register byte: bits 0-1 PRG, bits 4-7 CHR
        Gxrom({ bank : U8 }), # latched register byte: bits 4-5 PRG, bits 0-1 CHR
        Mmc1({ shift : U8, count : U8, control : U8, chr0 : U8, chr1 : U8, prg_bank : U8 }),
        Mmc2(
            {
                prg_bank : U8,
                # two 4K CHR windows, each with a bank register per latch value
                chr_fd0 : U8, # $B000: PT0 bank while latch0 = $FD
                chr_fe0 : U8, # $C000: PT0 bank while latch0 = $FE
                chr_fd1 : U8, # $D000: PT1 bank while latch1 = $FD
                chr_fe1 : U8, # $E000: PT1 bank while latch1 = $FE
                latch0 : U8, # $FD or $FE, set by PPU fetches of the trigger tiles
                latch1 : U8,
                mirroring : U8, # $F000 bit 0: 0 = vertical, 1 = horizontal
            },
        ),
        Mmc3(
            {
                bank_select : U8,
                banks : List(U8), # R0-R7
                mirroring : U8, # bit 0: 0 = vertical, 1 = horizontal
                irq_latch : U8,
                irq_counter : U8,
                irq_reload : Bool,
                irq_enabled : Bool,
                irq_asserted : Bool,
                a12_level : Bool, # PPU address line A12 as the MMC3 last saw it
                a12_fell : U64, # clock (absolute dots) when A12 last went low
            },
        ),
        Unsupported,
    ],
}.{
    # 64-bit FNV-1a over the ROM file; identifies which ROM a snapshot belongs to
    fnv1a : List(U8) -> U64
    fnv1a = |bytes|
        bytes.fold(0xCBF29CE484222325, |h, b| h.bitwise_xor(b.to_u64()).times_wrap(0x100000001B3))

    from_bytes : List(U8) -> Try(Cartridge, [NotANesFile])
    from_bytes = |bytes| {
        header = Header.parse(bytes)?
        trainer_len : U64
        trainer_len = if header.trainer { 512 } else { 0 }
        prg_start = trainer_len.plus(16)
        chr_rom = List.sublist(bytes, { start: prg_start.plus(header.prg_rom_size), len: header.chr_rom_size })
        mapper =
            match header.mapper {
                0 => Nrom
                1 => Mmc1({ shift: 0, count: 0, control: 0x0C, chr0: 0, chr1: 0, prg_bank: 0 })
                2 => Uxrom({ bank: 0 })
                3 => Cnrom({ bank: 0 })
                7 => Axrom({ bank: 0 })
                9 => Mmc2({ prg_bank: 0, chr_fd0: 0, chr_fe0: 0, chr_fd1: 0, chr_fe1: 0, latch0: 0xFE, latch1: 0xFE, mirroring: 0 })
                11 => ColorDreams({ bank: 0 })
                66 => Gxrom({ bank: 0 })
                4 =>
                    Mmc3({
                        bank_select: 0,
                        banks: List.repeat(0, 8),
                        mirroring: 0,
                        irq_latch: 0,
                        irq_counter: 0,
                        irq_reload: Bool.False,
                        irq_enabled: Bool.False,
                        irq_asserted: Bool.False,
                        a12_level: Bool.False,
                        a12_fell: 0,
                    })

                _ => Unsupported
            }
        cart : Cartridge
        cart = {
            header: header,
            hash: fnv1a(bytes),
            prg: List.sublist(bytes, { start: prg_start, len: header.prg_rom_size }),
            chr: if header.chr_rom_size == 0 { List.repeat(0, 8192) } else { chr_rom },
            chr_writable: header.chr_rom_size == 0,
            mapper: mapper,
        }
        Ok(cart)
    }

    # --- PRG (CPU 0x8000-0xFFFF) ---

    # whole-window 32K banking (AxROM / ColorDreams / GxROM)
    prg32_index : U64, U8, U64 -> U64
    prg32_index = |prg_len, bank, offset| {
        banks32 = prg_len.shr_zf_wrap(15)
        bank.to_u64().bitwise_and(banks32.minus(1)).shl_wrap(15).bitwise_or(offset)
    }

    read_prg : Cartridge, U16 -> U8
    read_prg = |cart, addr| {
        offset = addr.bitwise_and(0x7FFF).to_u64()
        prg_len = cart.prg.len()
        index =
            match cart.mapper {
                Uxrom(m) =>
                    if addr < 0xC000 {
                        # switchable 16K bank
                        bank = m.bank.to_u64().bitwise_and(prg_len.shr_zf_wrap(14).minus(1))
                        bank.shl_wrap(14).bitwise_or(offset.bitwise_and(0x3FFF))
                    } else {
                        # fixed last 16K bank
                        prg_len.minus(0x4000).plus(offset.bitwise_and(0x3FFF))
                    }

                Mmc1(m) => {
                    prg_mode = m.control.shr_zf_wrap(2).bitwise_and(0x03)
                    banks16 = prg_len.shr_zf_wrap(14)
                    if prg_mode < 2 {
                        # 32K mode
                        bank32 = m.prg_bank.shr_zf_wrap(1).to_u64().bitwise_and(banks16.shr_zf_wrap(1).minus(1))
                        bank32.shl_wrap(15).bitwise_or(offset)
                    } else if prg_mode == 2 {
                        # first bank fixed at 0x8000, switch 0xC000
                        if addr < 0xC000 {
                            offset.bitwise_and(0x3FFF)
                        } else {
                            bank = m.prg_bank.bitwise_and(0x0F).to_u64().bitwise_and(banks16.minus(1))
                            bank.shl_wrap(14).bitwise_or(offset.bitwise_and(0x3FFF))
                        }
                    } else {
                        # switch 0x8000, last bank fixed at 0xC000
                        if addr < 0xC000 {
                            bank = m.prg_bank.bitwise_and(0x0F).to_u64().bitwise_and(banks16.minus(1))
                            bank.shl_wrap(14).bitwise_or(offset.bitwise_and(0x3FFF))
                        } else {
                            prg_len.minus(0x4000).plus(offset.bitwise_and(0x3FFF))
                        }
                    }
                }

                Mmc3(m) => {
                    banks8 = prg_len.shr_zf_wrap(13)
                    slot = offset.shr_zf_wrap(13) # 0..3
                    prg_mode1 = m.bank_select.bitwise_and(0x40) != 0
                    r6 = (m.banks.get(6) ?? 0).to_u64()
                    r7 = (m.banks.get(7) ?? 0).to_u64()
                    bank =
                        if slot == 0 {
                            if prg_mode1 { banks8.minus(2) } else { r6 }
                        } else if slot == 1 {
                            r7
                        } else if slot == 2 {
                            if prg_mode1 { r6 } else { banks8.minus(2) }
                        } else {
                            banks8.minus(1)
                        }
                    bank.bitwise_and(banks8.minus(1)).shl_wrap(13).bitwise_or(offset.bitwise_and(0x1FFF))
                }

                Mmc2(m) =>
                    if addr < 0xA000 {
                        # switchable 8K bank
                        bank = m.prg_bank.bitwise_and(0x0F).to_u64().bitwise_and(prg_len.shr_zf_wrap(13).minus(1))
                        bank.shl_wrap(13).bitwise_or(offset.bitwise_and(0x1FFF))
                    } else {
                        # last three 8K banks fixed at 0xA000-0xFFFF
                        prg_len.minus(0x6000).plus(offset.minus(0x2000))
                    }

                Axrom(m) => prg32_index(prg_len, m.bank.bitwise_and(0x07), offset)
                ColorDreams(m) => prg32_index(prg_len, m.bank.bitwise_and(0x03), offset)
                Gxrom(m) => prg32_index(prg_len, m.bank.shr_zf_wrap(4).bitwise_and(0x03), offset)

                # Nrom / Cnrom / Unsupported: direct, 16K mirrored
                _ =>
                    if prg_len <= 16384 {
                        offset.bitwise_and(0x3FFF)
                    } else {
                        offset
                    }
            }
        cart.prg.get(index) ?? 0
    }

    # register writes in 0x8000-0xFFFF
    write_prg : Cartridge, U16, U8 -> Cartridge
    write_prg = |cart, addr, value|
        match cart.mapper {
            Uxrom(m) => { ..cart, mapper: Uxrom({ ..m, bank: value }) }
            Cnrom(m) => { ..cart, mapper: Cnrom({ ..m, bank: value }) }
            Axrom(m) => { ..cart, mapper: Axrom({ ..m, bank: value }) } # AOROM: no bus conflicts
            # discrete-logic boards: bus conflict ANDs the write with the ROM byte
            ColorDreams(m) => { ..cart, mapper: ColorDreams({ ..m, bank: value.bitwise_and(cart.read_prg(addr)) }) }
            Gxrom(m) => { ..cart, mapper: Gxrom({ ..m, bank: value.bitwise_and(cart.read_prg(addr)) }) }
            Mmc1(m) =>
                if value.bitwise_and(0x80) != 0 {
                    { ..cart, mapper: Mmc1({ ..m, shift: 0, count: 0, control: m.control.bitwise_or(0x0C) }) }
                } else {
                    loaded = m.shift.bitwise_or(value.bitwise_and(0x01).shl_wrap(m.count))
                    if m.count == 4 {
                        # fifth write routes by address quadrant
                        routed =
                            if addr < 0xA000 {
                                { ..m, control: loaded }
                            } else if addr < 0xC000 {
                                { ..m, chr0: loaded }
                            } else if addr < 0xE000 {
                                { ..m, chr1: loaded }
                            } else {
                                { ..m, prg_bank: loaded }
                            }
                        { ..cart, mapper: Mmc1({ ..routed, shift: 0, count: 0 }) }
                    } else {
                        { ..cart, mapper: Mmc1({ ..m, shift: loaded, count: m.count.plus_wrap(1) }) }
                    }
                }

            Mmc2(m) => {
                # registers decode in 4K windows; 0x8000-0x9FFF has none
                new_m =
                    if addr < 0xA000 {
                        m
                    } else if addr < 0xB000 {
                        { ..m, prg_bank: value.bitwise_and(0x0F) }
                    } else if addr < 0xC000 {
                        { ..m, chr_fd0: value.bitwise_and(0x1F) }
                    } else if addr < 0xD000 {
                        { ..m, chr_fe0: value.bitwise_and(0x1F) }
                    } else if addr < 0xE000 {
                        { ..m, chr_fd1: value.bitwise_and(0x1F) }
                    } else if addr < 0xF000 {
                        { ..m, chr_fe1: value.bitwise_and(0x1F) }
                    } else {
                        { ..m, mirroring: value.bitwise_and(0x01) }
                    }
                { ..cart, mapper: Mmc2(new_m) }
            }

            Mmc3(m) => {
                even = addr.bitwise_and(0x0001) == 0
                new_m =
                    if addr < 0xA000 {
                        if even {
                            { ..m, bank_select: value }
                        } else {
                            target = m.bank_select.bitwise_and(0x07).to_u64()
                            { ..m, banks: m.banks.set(target, value) ?? m.banks }
                        }
                    } else if addr < 0xC000 {
                        if even {
                            { ..m, mirroring: value }
                        } else {
                            m # PRG RAM protect: ignored
                        }
                    } else if addr < 0xE000 {
                        if even {
                            { ..m, irq_latch: value }
                        } else {
                            { ..m, irq_reload: Bool.True }
                        }
                    } else {
                        if even {
                            { ..m, irq_enabled: Bool.False, irq_asserted: Bool.False }
                        } else {
                            { ..m, irq_enabled: Bool.True }
                        }
                    }
                { ..cart, mapper: Mmc3(new_m) }
            }

            _ => cart
        }

    # --- CHR (PPU 0x0000-0x1FFF) ---

    # MMC2: each 4K window's bank register is picked by that window's latch
    mmc2_chr_index = |chr_len, m, l0, l1, rel| {
        banks4 = chr_len.shr_zf_wrap(12)
        reg =
            if rel < 0x1000 {
                if l0 == 0xFD { m.chr_fd0 } else { m.chr_fe0 }
            } else {
                if l1 == 0xFD { m.chr_fd1 } else { m.chr_fe1 }
            }
        reg.to_u64().bitwise_and(banks4.minus(1)).shl_wrap(12).bitwise_or(rel.bitwise_and(0x0FFF))
    }

    # MMC2 latch triggers, applied AFTER the read returns data: $0FD8/$0FE8
    # are exact addresses, the PT1 triggers are 8-byte ranges (the boards
    # decode PT0 and PT1 differently)
    mmc2_trigger = |latches, rel|
        if rel == 0x0FD8 {
            { ..latches, l0: 0xFD }
        } else if rel == 0x0FE8 {
            { ..latches, l0: 0xFE }
        } else if rel >= 0x1FD8 and rel <= 0x1FDF {
            { ..latches, l1: 0xFD }
        } else if rel >= 0x1FE8 and rel <= 0x1FEF {
            { ..latches, l1: 0xFE }
        } else {
            latches
        }

    # the mapper's committed latch state (identity for non-MMC2 mappers,
    # so callers can thread latches unconditionally)
    chr_latches : Cartridge -> { l0 : U8, l1 : U8 }
    chr_latches = |cart|
        match cart.mapper {
            Mmc2(m) => { l0: m.latch0, l1: m.latch1 }
            _ => { l0: 0xFE, l1: 0xFE }
        }

    set_chr_latches : Cartridge, { l0 : U8, l1 : U8 } -> Cartridge
    set_chr_latches = |cart, latches|
        match cart.mapper {
            Mmc2(m) => { ..cart, mapper: Mmc2({ ..m, latch0: latches.l0, latch1: latches.l1 }) }
            _ => cart
        }

    # fetch-observing pattern read: like read_chr, but the caller threads
    # latch state through consecutive fetches so an MMC2 latch flip takes
    # effect for the very next fetch (the same-scanline CHR split). The
    # cartridge itself stays immutable; the final latches are committed
    # via set_chr_latches when the render returns to the bus.
    chr_fetch : Cartridge, { l0 : U8, l1 : U8 }, U16 -> { value : U8, latches : { l0 : U8, l1 : U8 } }
    chr_fetch = |cart, latches, addr|
        match cart.mapper {
            Mmc2(m) => {
                rel = addr.bitwise_and(0x1FFF).to_u64()
                value = cart.chr.get(mmc2_chr_index(cart.chr.len(), m, latches.l0, latches.l1, rel)) ?? 0
                { value: value, latches: mmc2_trigger(latches, rel) }
            }

            _ => { value: cart.read_chr(addr), latches: latches }
        }

    read_chr : Cartridge, U16 -> U8
    read_chr = |cart, addr| {
        rel = addr.bitwise_and(0x1FFF).to_u64()
        chr_len = cart.chr.len()
        index =
            match cart.mapper {
                Cnrom(m) => {
                    banks8 = chr_len.shr_zf_wrap(13)
                    m.bank.to_u64().bitwise_and(banks8.minus(1)).shl_wrap(13).bitwise_or(rel)
                }

                Mmc1(m) =>
                    if m.control.bitwise_and(0x10) == 0 {
                        # 8K mode
                        banks8 = chr_len.shr_zf_wrap(13)
                        bank = m.chr0.shr_zf_wrap(1).to_u64().bitwise_and(if banks8 == 0 { 0 } else { banks8.minus(1) })
                        bank.shl_wrap(13).bitwise_or(rel)
                    } else {
                        # two 4K windows
                        banks4 = chr_len.shr_zf_wrap(12)
                        reg = if rel < 0x1000 { m.chr0 } else { m.chr1 }
                        bank = reg.to_u64().bitwise_and(if banks4 == 0 { 0 } else { banks4.minus(1) })
                        bank.shl_wrap(12).bitwise_or(rel.bitwise_and(0x0FFF))
                    }

                Mmc3(m) => {
                    banks1 = chr_len.shr_zf_wrap(10)
                    region = rel.shr_zf_wrap(10) # 1K regions 0..7
                    chr_mode1 = m.bank_select.bitwise_and(0x80) != 0
                    reg_of = |i| (m.banks.get(i) ?? 0).to_u64()
                    # mode 0: 2K windows (R0,R1) low, 1K windows (R2-R5) high; mode 1 swaps halves
                    logical = if chr_mode1 { region.bitwise_xor(4) } else { region }
                    bank1k =
                        if logical < 2 {
                            reg_of(0).bitwise_and(0xFE).plus(logical.bitwise_and(1))
                        } else if logical < 4 {
                            reg_of(1).bitwise_and(0xFE).plus(logical.bitwise_and(1))
                        } else {
                            reg_of(logical.minus(2)) # regions 4..7 -> R2..R5
                        }
                    bank1k.bitwise_and(banks1.minus(1)).shl_wrap(10).bitwise_or(rel.bitwise_and(0x03FF))
                }

                Mmc2(m) => mmc2_chr_index(chr_len, m, m.latch0, m.latch1, rel)

                ColorDreams(m) => {
                    banks8 = chr_len.shr_zf_wrap(13)
                    m.bank.shr_zf_wrap(4).to_u64().bitwise_and(banks8.minus(1)).shl_wrap(13).bitwise_or(rel)
                }

                Gxrom(m) => {
                    banks8 = chr_len.shr_zf_wrap(13)
                    m.bank.bitwise_and(0x03).to_u64().bitwise_and(banks8.minus(1)).shl_wrap(13).bitwise_or(rel)
                }

                # Nrom / Uxrom / Axrom / Unsupported: direct 8K
                _ => rel
            }
        cart.chr.get(index) ?? 0
    }

    # PPUDATA writes into pattern space land in CHR RAM (unbanked 8K in scope)
    write_chr : Cartridge, U16, U8 -> Cartridge
    write_chr = |cart, addr, value|
        if cart.chr_writable {
            { ..cart, chr: cart.chr.set(addr.bitwise_and(0x1FFF).to_u64(), value) ?? cart.chr }
        } else {
            cart
        }

    # --- mirroring / IRQ ---

    current_mirroring : Cartridge -> [Horizontal, Vertical, FourScreen, SingleLow, SingleHigh]
    current_mirroring = |cart|
        match cart.mapper {
            Mmc1(m) =>
                match m.control.bitwise_and(0x03) {
                    0 => SingleLow
                    1 => SingleHigh
                    2 => Vertical
                    _ => Horizontal
                }

            Mmc3(m) => if m.mirroring.bitwise_and(0x01) != 0 { Horizontal } else { Vertical }
            Mmc2(m) => if m.mirroring.bitwise_and(0x01) != 0 { Horizontal } else { Vertical }
            Axrom(m) => if m.bank.bitwise_and(0x10) != 0 { SingleHigh } else { SingleLow }
            _ =>
                match cart.header.mirroring {
                    Horizontal => Horizontal
                    Vertical => Vertical
                    FourScreen => FourScreen
                }
        }

    # current level of the mapper's IRQ line (asserted until acked)
    irq_line : Cartridge -> Bool
    irq_line = |cart|
        match cart.mapper {
            Mmc3(m) => m.irq_asserted
            _ => Bool.False
        }

    # one MMC3 IRQ-counter clock; irq is the level (kept as the counter
    # core - A12 machinery below decides WHEN it runs)
    clock_scanline : Cartridge -> { cart : Cartridge, irq : Bool }
    clock_scanline = |cart|
        match cart.mapper {
            Mmc3(m) => {
                new_counter =
                    if m.irq_counter == 0 or m.irq_reload {
                        m.irq_latch
                    } else {
                        m.irq_counter.minus_wrap(1)
                    }
                asserted = m.irq_asserted or (new_counter == 0 and m.irq_enabled)
                new_m = { ..m, irq_counter: new_counter, irq_reload: Bool.False, irq_asserted: asserted }
                { cart: { ..cart, mapper: Mmc3(new_m) }, irq: asserted }
            }

            _ => { cart: cart, irq: Bool.False }
        }

    # A12 rise at `at_clock` (absolute dots): clocks the counter iff the
    # line was low long enough (the MMC3's low-time filter, ~3 CPU cycles)
    a12_rise : Cartridge, U64 -> Cartridge
    a12_rise = |cart, at_clock|
        match cart.mapper {
            Mmc3(m) =>
                if m.a12_level {
                    cart # already high: not an edge
                } else {
                    lifted = { ..cart, mapper: Mmc3({ ..m, a12_level: Bool.True }) }
                    if at_clock >= m.a12_fell.plus(9) {
                        clock_scanline(lifted).cart
                    } else {
                        lifted # filtered: low time too short
                    }
                }

            _ => cart
        }

    # A12 fall at `at_clock`: starts the filter's low-time measurement
    a12_fall : Cartridge, U64 -> Cartridge
    a12_fall = |cart, at_clock|
        match cart.mapper {
            Mmc3(m) =>
                if m.a12_level {
                    { ..cart, mapper: Mmc3({ ..m, a12_level: Bool.False, a12_fell: at_clock }) }
                } else {
                    cart
                }

            _ => cart
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

# builds a synthetic PRG where every 8K bank is filled with its bank number
bank_marked_prg : U64, U64 -> List(U8)
bank_marked_prg = |bank_size, count| {
    build = |acc, i|
        if i >= count {
            acc
        } else {
            build(acc.concat(List.repeat(i.to_u8_wrap(), bank_size)), i.plus(1))
        }
    z : U64
    z = 0
    build([], z)
}

# MMC3: vectors from the last bank; R6 switches the 0x8000 slot; mode bit swaps
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 4, 1, 0x40, 0x00, 0, 0, 0, 0, 0, 0, 0, 0] # 64K PRG, mapper 4
    rom = header.concat(bank_marked_prg(8192, 8)).concat(List.repeat(0, 8192))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            switched = cart.write_prg(0x8000, 6).write_prg(0x8001, 3) # R6 = bank 3
            mode1 = switched.write_prg(0x8000, 0x46) # PRG mode 1, target still R6
            cart.read_prg(0xFFFC) == 7 # fixed last bank
            and cart.read_prg(0xC000) == 6 # fixed second-to-last (mode 0)
            and switched.read_prg(0x8000) == 3 # R6 at 0x8000 (mode 0)
            and mode1.read_prg(0xC000) == 3 # R6 moves to 0xC000 (mode 1)
            and mode1.read_prg(0x8000) == 6 # second-to-last at 0x8000 (mode 1)
        }

        Err(_) => Bool.False
    }
}

# MMC3: 1K CHR window and mirroring register
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 4, 2, 0x41, 0x00, 0, 0, 0, 0, 0, 0, 0, 0] # mapper 4, 16K CHR, vertical
    chr = bank_marked_prg(1024, 16)
    rom = header.concat(bank_marked_prg(8192, 8)).concat(chr)
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            switched = cart.write_prg(0x8000, 2).write_prg(0x8001, 9) # R2 = 1K bank 9
            mirrored = cart.write_prg(0xA000, 1)
            switched.read_chr(0x1000) == 9 # R2 covers 0x1000-0x13FF in mode 0
            and Cartridge.current_mirroring(cart) == Vertical
            and Cartridge.current_mirroring(mirrored) == Horizontal
        }

        Err(_) => Bool.False
    }
}

# MMC3 IRQ counter: latch, reload, assert at zero, 0xE000 deasserts
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 4, 1, 0x40, 0x00, 0, 0, 0, 0, 0, 0, 0, 0] # mapper 4
    rom = header.concat(bank_marked_prg(8192, 8)).concat(List.repeat(0, 8192))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            armed = cart.write_prg(0xC000, 2).write_prg(0xC001, 0).write_prg(0xE001, 0)
            c1 = armed.clock_scanline() # reload -> 2
            c2 = c1.cart.clock_scanline() # 1
            c3 = c2.cart.clock_scanline() # 0 -> assert
            acked = c3.cart.write_prg(0xE000, 0)
            c4 = acked.clock_scanline() # reload, disabled -> no assert
            c1.irq == Bool.False
            and c2.irq == Bool.False
            and c3.irq == Bool.True
            and c4.irq == Bool.False
        }

        Err(_) => Bool.False
    }
}

# UxROM: switchable low bank, fixed last bank
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 4, 0, 0x20, 0x00, 0, 0, 0, 0, 0, 0, 0, 0] # mapper 2, 64K PRG
    rom = header.concat(bank_marked_prg(16384, 4))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            switched = cart.write_prg(0x8000, 2)
            switched.read_prg(0x8000) == 2 and switched.read_prg(0xC000) == 3 and cart.read_prg(0x8000) == 0
        }

        Err(_) => Bool.False
    }
}

# CNROM: 8K CHR banks
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 4, 0x30, 0x00, 0, 0, 0, 0, 0, 0, 0, 0] # mapper 3, 32K CHR
    rom = header.concat(List.repeat(0xEA, 16384)).concat(bank_marked_prg(8192, 4))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            switched = cart.write_prg(0x8000, 2)
            switched.read_chr(0x0000) == 2 and cart.read_chr(0x0000) == 0
        }

        Err(_) => Bool.False
    }
}

# MMC1: five serial writes load a register; PRG mode 3 fixes the last bank
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 4, 0, 0x10, 0x00, 0, 0, 0, 0, 0, 0, 0, 0] # mapper 1, 64K PRG
    rom = header.concat(bank_marked_prg(16384, 4))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            # load prg_bank = 2 via five writes to 0xE000 (bits LSB first: 0,1,0,0,0)
            loaded =
                cart
                    .write_prg(0xE000, 0)
                    .write_prg(0xE000, 1)
                    .write_prg(0xE000, 0)
                    .write_prg(0xE000, 0)
                    .write_prg(0xE000, 0)
            loaded.read_prg(0x8000) == 2 # switched bank (mode 3 default)
            and loaded.read_prg(0xC000) == 3 # fixed last
            and Cartridge.current_mirroring(cart) == SingleLow # control 0x0C -> mirroring bits 0
        }

        Err(_) => Bool.False
    }
}

# AxROM: 32K whole-window switch (vectors follow), single-screen select, no bus conflicts
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 16, 0, 0x70, 0x00, 0, 0, 0, 0, 0, 0, 0, 0] # mapper 7, 256K PRG, CHR RAM
    rom = header.concat(bank_marked_prg(32768, 8))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            switched = cart.write_prg(0x8000, 3)
            upper = cart.write_prg(0x8000, 0x13) # bank 3 + nametable bit; ROM byte is 0, so a conflict would zero this
            cart.read_prg(0x8000) == 0
            and switched.read_prg(0x8000) == 3
            and switched.read_prg(0xFFFC) == 3 # reset vector rides the 32K window
            and Cartridge.current_mirroring(cart) == SingleLow
            and Cartridge.current_mirroring(upper) == SingleHigh
        }

        Err(_) => Bool.False
    }
}

# GxROM: one write switches PRG (bits 4-5) and CHR (bits 0-1) independently
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 8, 4, 0x20, 0x40, 0, 0, 0, 0, 0, 0, 0, 0] # mapper 66, 128K PRG, 32K CHR
    prg = bank_marked_prg(32768, 4).set(0x0100, 0x21) ?? [] # conflict-safe write site
    rom = header.concat(prg).concat(bank_marked_prg(8192, 4))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            switched = cart.write_prg(0x8100, 0x21) # PRG bank 2, CHR bank 1
            switched.read_prg(0x8000) == 2
            and switched.read_chr(0x0000) == 1
            and cart.read_prg(0x8000) == 0
            and cart.read_chr(0x0000) == 0
        }

        Err(_) => Bool.False
    }
}

# ColorDreams: swapped nibbles (PRG bits 0-1, CHR bits 4-7); bus conflict ANDs the write
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 4, 4, 0xB0, 0x00, 0, 0, 0, 0, 0, 0, 0, 0] # mapper 11, 64K PRG, 32K CHR
    prg = bank_marked_prg(32768, 2).set(0x0040, 0x21) ?? []
    rom = header.concat(prg).concat(bank_marked_prg(8192, 4))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            switched = cart.write_prg(0x8040, 0xFF) # ROM byte 0x21: conflict latches 0x21
            switched.read_prg(0x8000) == 1 # PRG bits 0-1
            and switched.read_chr(0x0000) == 2 # CHR bits 4-7
        }

        Err(_) => Bool.False
    }
}

# MMC2: switchable 8K at 0x8000, last three 8K banks fixed; $F000 mirroring
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 8, 4, 0x90, 0x00, 0, 0, 0, 0, 0, 0, 0, 0] # mapper 9, 128K PRG, 32K CHR
    rom = header.concat(bank_marked_prg(8192, 16)).concat(bank_marked_prg(4096, 8))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            switched = cart.write_prg(0xA000, 5)
            mirrored = cart.write_prg(0xF000, 1)
            cart.read_prg(0x8000) == 0
            and switched.read_prg(0x8000) == 5
            and switched.read_prg(0xA000) == 13 # last three 8K banks fixed
            and switched.read_prg(0xC000) == 14
            and switched.read_prg(0xFFFC) == 15 # vectors from the last bank
            and Cartridge.current_mirroring(cart) == Vertical
            and Cartridge.current_mirroring(mirrored) == Horizontal
        }

        Err(_) => Bool.False
    }
}

# MMC2: each 4K CHR window picks its bank register by the window's latch
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 8, 4, 0x90, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    rom = header.concat(bank_marked_prg(8192, 16)).concat(bank_marked_prg(4096, 8))
    match Cartridge.from_bytes(rom) {
        Ok(cart0) => {
            cart = cart0.write_prg(0xB000, 1).write_prg(0xC000, 2).write_prg(0xD000, 3).write_prg(0xE000, 4)
            flipped = cart.set_chr_latches({ l0: 0xFD, l1: 0xFD })
            # power-on latches are $FE: the $C000/$E000 banks are live
            cart.read_chr(0x0000) == 2
            and cart.read_chr(0x1000) == 4
            and flipped.read_chr(0x0000) == 1 # $B000 bank while latch0 = $FD
            and flipped.read_chr(0x1000) == 3 # $D000 bank while latch1 = $FD
        }

        Err(_) => Bool.False
    }
}

# MMC2 latch triggers: exact PT0 addresses, 8-byte PT1 ranges, and the
# read returns data from the bank selected BEFORE the latch updates
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 8, 4, 0x90, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    rom = header.concat(bank_marked_prg(8192, 16)).concat(bank_marked_prg(4096, 8))
    match Cartridge.from_bytes(rom) {
        Ok(cart0) => {
            cart = cart0.write_prg(0xB000, 1).write_prg(0xC000, 2).write_prg(0xD000, 3).write_prg(0xE000, 4)
            lat = cart.chr_latches() # power-on { l0: 0xFE, l1: 0xFE }
            f1 = cart.chr_fetch(lat, 0x0FD8) # trigger: data from the FE bank, then latch0 -> FD
            f2 = cart.chr_fetch(f1.latches, 0x0000) # the very next PT0 fetch reads the FD bank
            f3 = cart.chr_fetch(f1.latches, 0x0FD8) # idempotent re-trigger, now reading the FD bank
            f4 = cart.chr_fetch(lat, 0x0FD9) # inside tile $FD but not the trigger byte
            f5 = cart.chr_fetch(lat, 0x1FDF) # PT1 triggers are ranges
            f6 = cart.chr_fetch({ l0: 0xFE, l1: 0xFD }, 0x1FEC)
            f1.value == 2
            and f1.latches.l0 == 0xFD
            and f2.value == 1
            and f3.value == 1
            and f3.latches.l0 == 0xFD
            and f4.latches.l0 == 0xFE
            and f5.latches.l1 == 0xFD
            and f6.latches.l1 == 0xFE
        }

        Err(_) => Bool.False
    }
}

# CHR RAM round-trips when the header reports no CHR ROM
expect {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 0, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    rom = header.concat(List.repeat(0xEA, 16384))
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            written = cart.write_chr(0x0155, 0x77)
            written.read_chr(0x0155) == 0x77 and cart.chr_writable == Bool.True
        }

        Err(_) => Bool.False
    }
}
