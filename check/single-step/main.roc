app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    nes: "../../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import nes.Cpu
import nes.Memory
import nes.Register

# Pure-Roc runner for Tom Harte's SingleStepTests (65x02 / nes6502).
#
#   roc check/single-step/main.roc -- check/single-step/data/*.json
#
# Each JSON file holds ~10,000 generated cases for one opcode:
# an initial CPU+memory state, the expected final state, and the
# cycle-by-cycle bus activity (we verify its length = total cycles).
# The parser below is a minimal recursive-descent pass over the fixed,
# machine-generated schema - no external JSON dependency.

State : { pc : U16, s : U8, a : U8, x : U8, y : U8, p : U8, ram : List({ addr : U16, val : U8 }) }

Case : { initial : State, final : State, cycles : U64 }

# --- minimal JSON parsing over the fixed schema ---

mul10 : U64 -> U64
mul10 = |n| n.shl_wrap(3).plus(n.shl_wrap(1))

skip_ws : List(U8), U64 -> U64
skip_ws = |b, i|
    match b.get(i) {
        Ok(c) =>
            if c == 32 or c == 10 or c == 13 or c == 9 {
                skip_ws(b, i.plus(1))
            } else {
                i
            }

        Err(_) => i
    }

expect_ch : List(U8), U64, U8 -> Try(U64, [ParseError(U64)])
expect_ch = |b, i0, ch| {
    i = skip_ws(b, i0)
    if (b.get(i) ?? 0) == ch {
        Ok(i.plus(1))
    } else {
        Err(ParseError(i))
    }
}

parse_u64 : List(U8), U64 -> Try({ i : U64, val : U64 }, [ParseError(U64)])
parse_u64 = |b, i0| parse_digits(b, skip_ws(b, i0), 0, Bool.False)

parse_digits : List(U8), U64, U64, Bool -> Try({ i : U64, val : U64 }, [ParseError(U64)])
parse_digits = |b, i, acc, any|
    match b.get(i) {
        Ok(c) =>
            if c >= 48 and c <= 57 {
                parse_digits(b, i.plus(1), mul10(acc).plus(c.minus(48).to_u64()), Bool.True)
            } else if any {
                Ok({ i: i, val: acc })
            } else {
                Err(ParseError(i))
            }

        Err(_) =>
            if any {
                Ok({ i: i, val: acc })
            } else {
                Err(ParseError(i))
            }
    }

# position after the closing quote of a string whose opening quote is at/after i0
skip_string : List(U8), U64 -> Try(U64, [ParseError(U64)])
skip_string = |b, i0| {
    i = expect_ch(b, i0, 34)?
    skip_to_quote(b, i)
}

skip_to_quote : List(U8), U64 -> Try(U64, [ParseError(U64)])
skip_to_quote = |b, i|
    match b.get(i) {
        Ok(34) => Ok(i.plus(1))
        Ok(_) => skip_to_quote(b, i.plus(1))
        Err(_) => Err(ParseError(i))
    }

# object keys, identified by their first byte(s); the schema has no others
parse_key : List(U8), U64 -> Try({ i : U64, val : [KName, KInitial, KFinal, KCycles, KPc, KS, KA, KX, KY, KP, KRam, KOther] }, [ParseError(U64)])
parse_key = |b, i0| {
    i1 = expect_ch(b, i0, 34)?
    c1 = b.get(i1) ?? 0
    c2 = b.get(i1.plus(1)) ?? 0
    key =
        if c1 == 110 {
            KName
        } else if c1 == 105 {
            KInitial
        } else if c1 == 102 {
            KFinal
        } else if c1 == 99 {
            KCycles
        } else if c1 == 112 and c2 == 99 {
            KPc
        } else if c1 == 112 {
            KP
        } else if c1 == 115 {
            KS
        } else if c1 == 97 {
            KA
        } else if c1 == 120 {
            KX
        } else if c1 == 121 {
            KY
        } else if c1 == 114 {
            KRam
        } else {
            KOther
        }
    i2 = skip_to_quote(b, i1)?
    Ok({ i: i2, val: key })
}

parse_ram : List(U8), U64 -> Try({ i : U64, val : List({ addr : U16, val : U8 }) }, [ParseError(U64)])
parse_ram = |b, i0| {
    i = expect_ch(b, i0, 91)?
    parse_ram_items(b, i, [])
}

parse_ram_items : List(U8), U64, List({ addr : U16, val : U8 }) -> Try({ i : U64, val : List({ addr : U16, val : U8 }) }, [ParseError(U64)])
parse_ram_items = |b, i0, acc| {
    i = skip_ws(b, i0)
    match b.get(i) {
        Ok(93) => Ok({ i: i.plus(1), val: acc })
        Ok(44) => parse_ram_items(b, i.plus(1), acc)
        Ok(91) => {
            a = parse_u64(b, i.plus(1))?
            i2 = expect_ch(b, a.i, 44)?
            v = parse_u64(b, i2)?
            i3 = expect_ch(b, v.i, 93)?
            parse_ram_items(b, i3, acc.append({ addr: a.val.to_u16_wrap(), val: v.val.to_u8_wrap() }))
        }

        _ => Err(ParseError(i))
    }
}

# count the entries of the cycles array; contents are skipped
parse_cycles : List(U8), U64 -> Try({ i : U64, val : U64 }, [ParseError(U64)])
parse_cycles = |b, i0| {
    i = expect_ch(b, i0, 91)?
    count_cycles(b, i, 0)
}

count_cycles : List(U8), U64, U64 -> Try({ i : U64, val : U64 }, [ParseError(U64)])
count_cycles = |b, i0, n| {
    i = skip_ws(b, i0)
    match b.get(i) {
        Ok(93) => Ok({ i: i.plus(1), val: n })
        Ok(44) => count_cycles(b, i.plus(1), n)
        Ok(91) => {
            i2 = skip_bracket(b, i.plus(1))?
            count_cycles(b, i2, n.plus(1))
        }

        _ => Err(ParseError(i))
    }
}

# scan to the matching ']' of a cycle triple ("read"/"write" contain no ']')
skip_bracket : List(U8), U64 -> Try(U64, [ParseError(U64)])
skip_bracket = |b, i|
    match b.get(i) {
        Ok(93) => Ok(i.plus(1))
        Ok(_) => skip_bracket(b, i.plus(1))
        Err(_) => Err(ParseError(i))
    }

empty_state : {} -> State
empty_state = |_| { pc: 0, s: 0, a: 0, x: 0, y: 0, p: 0, ram: [] }

parse_state : List(U8), U64 -> Try({ i : U64, val : State }, [ParseError(U64)])
parse_state = |b, i0| {
    i = expect_ch(b, i0, 123)?
    parse_state_fields(b, i, empty_state({}))
}

parse_state_fields : List(U8), U64, State -> Try({ i : U64, val : State }, [ParseError(U64)])
parse_state_fields = |b, i0, st| {
    i = skip_ws(b, i0)
    match b.get(i) {
        Ok(125) => Ok({ i: i.plus(1), val: st })
        Ok(44) => parse_state_fields(b, i.plus(1), st)
        Ok(34) => {
            k = parse_key(b, i)?
            i2 = expect_ch(b, k.i, 58)?
            match k.val {
                KPc => {
                    r = parse_u64(b, i2)?
                    parse_state_fields(b, r.i, { ..st, pc: r.val.to_u16_wrap() })
                }

                KS => {
                    r = parse_u64(b, i2)?
                    parse_state_fields(b, r.i, { ..st, s: r.val.to_u8_wrap() })
                }

                KA => {
                    r = parse_u64(b, i2)?
                    parse_state_fields(b, r.i, { ..st, a: r.val.to_u8_wrap() })
                }

                KX => {
                    r = parse_u64(b, i2)?
                    parse_state_fields(b, r.i, { ..st, x: r.val.to_u8_wrap() })
                }

                KY => {
                    r = parse_u64(b, i2)?
                    parse_state_fields(b, r.i, { ..st, y: r.val.to_u8_wrap() })
                }

                KP => {
                    r = parse_u64(b, i2)?
                    parse_state_fields(b, r.i, { ..st, p: r.val.to_u8_wrap() })
                }

                KRam => {
                    r = parse_ram(b, i2)?
                    parse_state_fields(b, r.i, { ..st, ram: r.val })
                }

                _ => Err(ParseError(i))
            }
        }

        _ => Err(ParseError(i))
    }
}

parse_case : List(U8), U64 -> Try({ i : U64, val : Case }, [ParseError(U64)])
parse_case = |b, i0| {
    i = expect_ch(b, i0, 123)?
    parse_case_fields(b, i, { initial: empty_state({}), final: empty_state({}), cycles: 0 })
}

parse_case_fields : List(U8), U64, Case -> Try({ i : U64, val : Case }, [ParseError(U64)])
parse_case_fields = |b, i0, case| {
    i = skip_ws(b, i0)
    match b.get(i) {
        Ok(125) => Ok({ i: i.plus(1), val: case })
        Ok(44) => parse_case_fields(b, i.plus(1), case)
        Ok(34) => {
            k = parse_key(b, i)?
            i2 = expect_ch(b, k.i, 58)?
            match k.val {
                KName => {
                    i3 = skip_string(b, i2)?
                    parse_case_fields(b, i3, case)
                }

                KInitial => {
                    r = parse_state(b, i2)?
                    parse_case_fields(b, r.i, { ..case, initial: r.val })
                }

                KFinal => {
                    r = parse_state(b, i2)?
                    parse_case_fields(b, r.i, { ..case, final: r.val })
                }

                KCycles => {
                    r = parse_cycles(b, i2)?
                    parse_case_fields(b, r.i, { ..case, cycles: r.val })
                }

                _ => Err(ParseError(i))
            }
        }

        _ => Err(ParseError(i))
    }
}

parse_cases : List(U8) -> Try(List(Case), [ParseError(U64)])
parse_cases = |b| {
    i = expect_ch(b, 0, 91)?
    parse_case_list(b, i, [])
}

parse_case_list : List(U8), U64, List(Case) -> Try(List(Case), [ParseError(U64)])
parse_case_list = |b, i0, acc| {
    i = skip_ws(b, i0)
    match b.get(i) {
        Ok(93) => Ok(acc)
        Ok(44) => parse_case_list(b, i.plus(1), acc)
        Ok(123) => {
            r = parse_case(b, i)?
            parse_case_list(b, r.i, acc.append(r.val))
        }

        _ => Err(ParseError(i))
    }
}

# --- case execution ---

check_u8 : Str, U8, U8 -> Str
check_u8 = |label, got, want|
    if got == want {
        ""
    } else {
        "${label}: got ${got.to_str()} want ${want.to_str()}"
    }

check_u16 : Str, U16, U16 -> Str
check_u16 = |label, got, want|
    if got == want {
        ""
    } else {
        "${label}: got ${got.to_str()} want ${want.to_str()}"
    }

check_u64 : Str, U64, U64 -> Str
check_u64 = |label, got, want|
    if got == want {
        ""
    } else {
        "${label}: got ${got.to_str()} want ${want.to_str()}"
    }

run_case : Case -> Str
run_case = |case| {
    mem = case.initial.ram.fold(List.repeat(0, 0x10000), |m, e| Memory.write8(m, e.addr, e.val))
    reg =
        Register.init({})
            .write16(ProgramCounter, case.initial.pc)
            .write8(StackPointer, case.initial.s)
            .write8(Accumulator, case.initial.a)
            .write8(X, case.initial.x)
            .write8(Y, case.initial.y)
            .write8(Status, case.initial.p)
    done = Cpu.make(reg, mem).step()
    reg_checks = [
        check_u16("pc", done.reg.program_counter, case.final.pc),
        check_u8("s", done.reg.stack_pointer, case.final.s),
        check_u8("a", done.reg.accumulator, case.final.a),
        check_u8("x", done.reg.x, case.final.x),
        check_u8("y", done.reg.y, case.final.y),
        check_u8("p", done.reg.status, case.final.p),
        check_u64("cycles", done.cycles, case.cycles),
    ]
    ram_checks = case.final.ram.map(|e| check_u8("ram[${e.addr.to_str()}]", Memory.read8(done.mem, e.addr), e.val))
    reg_checks.concat(ram_checks).fold("", |acc, s| if s == "" { acc } else if acc == "" { s } else { "${acc}; ${s}" })
}

FileStats : { pass : U64, fail : U64, detail : Str, idx : U64 }

run_cases : List(Case) -> FileStats
run_cases = |cases|
    cases.fold({ pass: 0, fail: 0, detail: "", idx: 0 }, |acc, case| {
        msg = run_case(case)
        if msg == "" {
            { ..acc, pass: acc.pass.plus(1), idx: acc.idx.plus(1) }
        } else {
            detail =
                if acc.fail < 5 {
                    "${acc.detail}\n    case ${acc.idx.to_str()}: ${msg}"
                } else {
                    acc.detail
                }
            { ..acc, fail: acc.fail.plus(1), detail: detail, idx: acc.idx.plus(1) }
        }
    })

# --- driver ---

run_file! = |arg| {
    path = Path.from_os_str(arg)
    bytes = path.read_bytes!()?
    match parse_cases(bytes) {
        Ok(cases) => {
            stats = run_cases(cases)
            if stats.fail == 0 {
                Stdout.line!("${path.display()}: ok (${stats.pass.to_str()} cases)")?
            } else {
                Stdout.line!("${path.display()}: FAIL ${stats.fail.to_str()}/${stats.pass.plus(stats.fail).to_str()}${stats.detail}")?
            }
            Ok(stats.fail)
        }

        Err(ParseError(i)) => {
            Stdout.line!("${path.display()}: parse error at byte ${i.to_str()}")?
            Ok(1)
        }
    }
}

run_all! = |args, idx, failed|
    match args.get(idx) {
        Err(_) => Ok(failed)
        Ok(arg) => {
            n = run_file!(arg)?
            run_all!(args, idx.plus(1), failed.plus(n))
        }
    }

main! : List(OsStr) => Try({}, _)
main! = |args| {
    failed = run_all!(args, 1, 0)?
    if failed > 0 {
        Stdout.line!("FAILED: ${failed.to_str()} case(s)")?
        Err(CheckFailed)
    } else {
        Stdout.line!("all ok")?
        Ok({})
    }
}

# parser expects
expect parse_u64([32, 49, 50, 51, 44], 0) == Ok({ i: 4, val: 123 })
expect skip_ws([32, 9, 65], 0) == 2
expect {
    r = parse_ram(" [[5,7],[65535,255]]".to_utf8(), 0)
    r == Ok({ i: 20, val: [{ addr: 5, val: 7 }, { addr: 65535, val: 255 }] })
}
