app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    nes: "../../../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import nes.Cartridge
import nes.Disasm

# 6502 disassembler over an iNES ROM with explicit MMC3 banking; the pure-Roc
# replacement for tools/disasm6502.py (born in the SMB3 sprite hunt). Decoding
# goes through the core's verified table (all 256 opcodes) and bytes through
# `Cartridge.read_prg`, with bank flags applied as real MMC3 register writes —
# the tool cannot disagree with the emulator. Run from the repo root:
#
#   roc check/inspect/tools/disasm.roc -- <rom.nes> <cpu_addr_hex> \
#       [--bank8000 N] [--bankA000 N] [--bankC000 N]
#
# Defaults are MMC3 PRG mode 1: 0x8000 = second-to-last 8K bank, 0xE000 =
# last; --bank8000 switches to PRG mode 0 (real MMC3 cannot switch 0x8000
# and 0xC000 at once, so combining those two flags is an error). Prints 40
# `ADDR: MNEMONIC operand` lines. One deviation from the Python tool:
# operands carry proper indexed/indirect suffixes (`,X`, `($NN),Y`, ...).

usage : Str
usage = "usage: <rom.nes> <cpu_addr_hex> [--bank8000 N] [--bankA000 N] [--bankC000 N]"

parse_dec : List(U8), U64, U64 -> U64
parse_dec = |b, i, acc|
    match b.get(i) {
        Ok(c) =>
            if c >= 48 and c <= 57 {
                parse_dec(b, i.plus(1), acc.shl_wrap(3).plus(acc.shl_wrap(1)).plus(c.minus(48).to_u64()))
            } else {
                acc
            }

        Err(_) => acc
    }

# hex with optional "$" or "0x"/"0X" prefix; Err on empty or stray chars
parse_hex : Str -> Try(U64, [BadHex])
parse_hex = |s| {
    bytes = s.to_utf8()
    z : U64
    z = 0
    start =
        match bytes {
            [36, ..] => 1 # "$"
            [48, 120, ..] => 2 # "0x"
            [48, 88, ..] => 2 # "0X"
            _ => z
        }
    go = |i, acc|
        match bytes.get(i) {
            Ok(c) =>
                if c >= 48 and c <= 57 {
                    go(i.plus(1), acc.shl_wrap(4).plus(c.minus(48).to_u64()))
                } else if c >= 65 and c <= 70 {
                    go(i.plus(1), acc.shl_wrap(4).plus(c.minus(55).to_u64()))
                } else if c >= 97 and c <= 102 {
                    go(i.plus(1), acc.shl_wrap(4).plus(c.minus(87).to_u64()))
                } else {
                    Err(BadHex)
                }

            Err(_) => if i == start { Err(BadHex) } else { Ok(acc) }
        }
    go(start, z)
}

Banks : { b8000 : [None, Some(U8)], ba000 : [None, Some(U8)], bc000 : [None, Some(U8)] }

parse_flags : List(Str), Banks -> Try(Banks, _)
parse_flags = |args, acc|
    match args {
        [] => Ok(acc)
        [flag, value, .. as rest] => {
            n = parse_dec(value.to_utf8(), 0, 0).to_u8_wrap()
            if flag == "--bank8000" {
                parse_flags(rest, { ..acc, b8000: Some(n) })
            } else if flag == "--bankA000" {
                parse_flags(rest, { ..acc, ba000: Some(n) })
            } else if flag == "--bankC000" {
                parse_flags(rest, { ..acc, bc000: Some(n) })
            } else {
                Err(Usage(usage))
            }
        }

        _ => Err(Usage(usage))
    }

# Apply the requested windows by driving the real MMC3 registers:
# bank-select (PRG mode bit + R6/R7 target), then bank-data. Without
# --bank8000 we stay in PRG mode 1 (0x8000 fixed second-to-last, R6 at
# 0xC000); with it we use mode 0 (R6 at 0x8000, 0xC000 fixed).
apply_banks : Cartridge, Banks -> Try(Cartridge, _)
apply_banks = |cart, banks|
    match cart.mapper {
        Mmc3(_) => {
            match { m0: banks.b8000, m1: banks.bc000 } {
                { m0: Some(_), m1: Some(_) } =>
                    Err(Usage("--bank8000 and --bankC000 both switch R6; real MMC3 fixes one of those windows per PRG mode, pick one"))

                _ => {
                    mode_bit =
                        match banks.b8000 {
                            Some(_) => 0x00
                            None => 0x40
                        }
                    r6 =
                        match banks.b8000 {
                            Some(n) => n
                            None =>
                                match banks.bc000 {
                                    Some(n) => n
                                    None => 0
                                }
                        }
                    r7 =
                        match banks.ba000 {
                            Some(n) => n
                            None => 0
                        }
                    configured =
                        cart
                            .write_prg(0x8000, U8.bitwise_or(mode_bit, 6))
                            .write_prg(0x8001, r6)
                            .write_prg(0x8000, U8.bitwise_or(mode_bit, 7))
                            .write_prg(0x8001, r7)
                    Ok(configured)
                }
            }
        }

        _ =>
            match { m0: banks.b8000, m1: banks.ba000, m2: banks.bc000 } {
                { m0: None, m1: None, m2: None } => Ok(cart)
                _ => Err(Usage("bank flags need an MMC3 (mapper 4) ROM"))
            }
    }

print_all! = |lines, i|
    match lines.get(i) {
        Ok(l) => {
            Stdout.line!(l)?
            print_all!(lines, i.plus(1))
        }

        Err(_) => Ok({})
    }

main! : List(OsStr) => Try({}, _)
main! = |args| {
    match args {
        [_, rom_arg, addr_arg, .. as flag_args] => {
            rom_bytes = Path.from_os_str(rom_arg).read_bytes!()?
            start = parse_hex(Path.from_os_str(addr_arg).display()) ? |_| Usage(usage)
            flags = flag_args.fold([], |acc, a| acc.append(Path.from_os_str(a).display()))
            banks = parse_flags(flags, { b8000: None, ba000: None, bc000: None })?
            cart = Cartridge.from_bytes(rom_bytes) ? |_| NotANesFile
            configured = apply_banks(cart, banks)?
            z : U64
            z = 0
            print_all!(Disasm.listing(configured, start.to_u16_wrap(), 40), z)
        }

        _ => Err(Usage(usage))
    }
}
