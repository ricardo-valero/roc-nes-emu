app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    nes: "../../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import nes.Cpu
import nes.Bus
import nes.Cartridge
import nes.Register

# nestest golden-log conformance, run from the repo root:
#
#   roc check/nestest/fetch.roc     # once
#   roc check/nestest/main.roc
#
# Boots nestest.nes in automation mode (PC = 0xC000) and compares every
# instruction's PC/A/X/Y/P/SP and cumulative cycles against the canonical
# log. The log's cycle counter starts at 7 (post-reset); ours starts at 0,
# so the comparison adds the offset. PPU columns are ignored until the PPU
# capability exists.

Expected : { pc : U16, a : U8, x : U8, y : U8, p : U8, sp : U8, cyc : U64 }

# --- log parsing ---

hex_val : U8 -> U64
hex_val = |c|
    if c >= 48 and c <= 57 {
        c.minus(48).to_u64()
    } else if c >= 65 and c <= 70 {
        c.minus(55).to_u64()
    } else {
        0
    }

hex2_at : List(U8), U64 -> U8
hex2_at = |b, i| {
    hi = hex_val(b.get(i) ?? 0)
    lo = hex_val(b.get(i.plus(1)) ?? 0)
    hi.shl_wrap(4).bitwise_or(lo).to_u8_wrap()
}

hex4_at : List(U8), U64 -> U16
hex4_at = |b, i| {
    hi = hex2_at(b, i)
    lo = hex2_at(b, i.plus(2))
    hi.to_u16().shl_wrap(8).bitwise_or(lo.to_u16())
}

# find the next occurrence of two bytes, starting at i, up to line end
find_pair : List(U8), U64, U8, U8 -> Try(U64, [NotFound])
find_pair = |b, i, c1, c2|
    match b.get(i) {
        Err(_) => Err(NotFound)
        Ok(10) => Err(NotFound)
        Ok(c) =>
            if c == c1 and (b.get(i.plus(1)) ?? 0) == c2 {
                Ok(i)
            } else {
                find_pair(b, i.plus(1), c1, c2)
            }
    }

parse_decimal : List(U8), U64, U64 -> U64
parse_decimal = |b, i, acc|
    match b.get(i) {
        Ok(c) =>
            if c >= 48 and c <= 57 {
                parse_decimal(b, i.plus(1), acc.shl_wrap(3).plus(acc.shl_wrap(1)).plus(c.minus(48).to_u64()))
            } else {
                acc
            }

        Err(_) => acc
    }

line_end : List(U8), U64 -> U64
line_end = |b, i|
    match b.get(i) {
        Ok(10) => i
        Ok(_) => line_end(b, i.plus(1))
        Err(_) => i
    }

# parse one log line starting at i; Ok(None) for blank/short lines
parse_line : List(U8), U64 -> Try({ next : U64, val : [Some(Expected), Empty] }, [BadLine(U64)])
parse_line = |b, i| {
    end = line_end(b, i)
    if end.minus(i) < 10 {
        Ok({ next: end.plus(1), val: Empty })
    } else {
        a_pos = find_pair(b, i.plus(4), 65, 58) ? |_| BadLine(i) # "A:"
        cyc_pos = find_pair(b, a_pos, 67, 89) ? |_| BadLine(i) # "CY"
        expected : Expected
        expected = {
            pc: hex4_at(b, i),
            a: hex2_at(b, a_pos.plus(2)),
            x: hex2_at(b, a_pos.plus(7)),
            y: hex2_at(b, a_pos.plus(12)),
            p: hex2_at(b, a_pos.plus(17)),
            sp: hex2_at(b, a_pos.plus(23)),
            cyc: parse_decimal(b, cyc_pos.plus(4), 0),
        }
        Ok({ next: end.plus(1), val: Some(expected) })
    }
}

parse_log : List(U8), U64, List(Expected) -> Try(List(Expected), [BadLine(U64)])
parse_log = |b, i, acc|
    if i >= b.len() {
        Ok(acc)
    } else {
        match parse_line(b, i) {
            Ok(r) =>
                match r.val {
                    Some(e) => parse_log(b, r.next, acc.append(e))
                    Empty => parse_log(b, r.next, acc)
                }

            Err(e) => Err(e)
        }
    }

# --- comparison loop ---

hex_digit : U64 -> U8
hex_digit = |d| if d < 10 { d.to_u8_wrap().plus(48) } else { d.to_u8_wrap().plus(55) }

hex4_str : U16 -> Str
hex4_str = |n| {
    v = n.to_u64()
    b = [
        hex_digit(v.shr_zf_wrap(12).bitwise_and(15)),
        hex_digit(v.shr_zf_wrap(8).bitwise_and(15)),
        hex_digit(v.shr_zf_wrap(4).bitwise_and(15)),
        hex_digit(v.bitwise_and(15)),
    ]
    Str.from_utf8(b) ?? "????"
}

hex2_str : U8 -> Str
hex2_str = |n| {
    v = n.to_u64()
    b = [hex_digit(v.shr_zf_wrap(4).bitwise_and(15)), hex_digit(v.bitwise_and(15))]
    Str.from_utf8(b) ?? "??"
}

check_field : Str, U64, U64 -> Str
check_field = |label, got, want|
    if got == want {
        ""
    } else {
        "${label}: got ${got.to_str()} want ${want.to_str()}"
    }

compare_state : Cpu, Expected -> Str
compare_state = |cpu, e| {
    checks = [
        if cpu.reg.program_counter == e.pc { "" } else { "pc: got ${hex4_str(cpu.reg.program_counter)} want ${hex4_str(e.pc)}" },
        if cpu.reg.accumulator == e.a { "" } else { "a: got ${hex2_str(cpu.reg.accumulator)} want ${hex2_str(e.a)}" },
        if cpu.reg.x == e.x { "" } else { "x: got ${hex2_str(cpu.reg.x)} want ${hex2_str(e.x)}" },
        if cpu.reg.y == e.y { "" } else { "y: got ${hex2_str(cpu.reg.y)} want ${hex2_str(e.y)}" },
        if cpu.reg.status == e.p { "" } else { "p: got ${hex2_str(cpu.reg.status)} want ${hex2_str(e.p)}" },
        if cpu.reg.stack_pointer == e.sp { "" } else { "sp: got ${hex2_str(cpu.reg.stack_pointer)} want ${hex2_str(e.sp)}" },
        check_field("cyc", cpu.cycles.plus(7), e.cyc),
    ]
    checks.fold("", |acc, s| if s == "" { acc } else if acc == "" { s } else { "${acc}; ${s}" })
}

run_log : Cpu, List(Expected), U64 -> [AllMatched(U64), Diverged(U64, Str)]
run_log = |cpu, expected, idx|
    match expected.get(idx) {
        Err(_) => AllMatched(idx)
        Ok(e) => {
            msg = compare_state(cpu, e)
            if msg == "" {
                run_log(cpu.step(), expected, idx.plus(1))
            } else {
                Diverged(idx.plus(1), msg)
            }
        }
    }

main! : List(OsStr) => Try({}, _)
main! = |_args| {
    rom_bytes = Path.from_os_str(OsStr.from_str("check/nestest/data/nestest.nes")).read_bytes!()?
    log_bytes = Path.from_os_str(OsStr.from_str("check/nestest/data/nestest.log")).read_bytes!()?
    cart = Cartridge.from_bytes(rom_bytes) ? |_| NotANesFile
    expected = parse_log(log_bytes, 0, []) ? |BadLine(i)| BadLogLine(i)
    reg = Register.init({}).write16(ProgramCounter, 0xC000)
    cpu = Cpu.make(reg, Bus.from_cartridge(cart))
    match run_log(cpu, expected, 0) {
        AllMatched(n) => Stdout.line!("nestest: all ${n.to_str()} log lines matched")
        Diverged(line, msg) => {
            Stdout.line!("nestest: DIVERGED at log line ${line.to_str()}: ${msg}")?
            Err(NestestDiverged)
        }
    }
}
