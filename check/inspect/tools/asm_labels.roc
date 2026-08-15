app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
}

import pf.OsStr
import pf.Path
import pf.Stdout

# Map RAM labels from the captainsouthbird/smb3 disassembly to addresses;
# the pure-Roc replacement for tools/asm_labels.py. Run from the repo root:
#
#   roc check/inspect/tools/asm_labels.roc -- <smb3.asm> [label ...]
#
# `.org $HHHH` sets the current address; every `.ds N` line advances it —
# BOTH labeled (`Label: .ds N`) and anonymous ones. Anonymous gaps shift
# everything after them (the hard-won lesson of the SMB3 sprite hunt).
# Labeled lines print `Label = $ADDR (N)`; trailing args filter by label,
# no args dumps all labels.

is_ws : U8 -> Bool
is_ws = |c| c == 32 or c == 9 or c == 13

# \w of the Python regex: [A-Za-z0-9_]
is_word : U8 -> Bool
is_word = |c|
    (c >= 65 and c <= 90) or (c >= 97 and c <= 122) or (c >= 48 and c <= 57) or c == 95

skip_ws : List(U8), U64 -> U64
skip_ws = |b, i|
    match b.get(i) {
        Ok(c) => if is_ws(c) { skip_ws(b, i.plus(1)) } else { i }
        Err(_) => i
    }

# literal match, returning the index just past it
match_lit : List(U8), U64, Str -> [NoMatch, Matched(U64)]
match_lit = |b, i, lit| {
    lb = lit.to_utf8()
    go = |j|
        match lb.get(j) {
            Ok(c) =>
                if (b.get(i.plus(j)) ?? 0) == c {
                    go(j.plus(1))
                } else {
                    NoMatch
                }

            Err(_) => Matched(i.plus(lb.len()))
        }
    z : U64
    z = 0
    go(z)
}

# at least one hex digit from i
parse_hex : List(U8), U64 -> [NoMatch, Parsed({ value : U64, next : U64 })]
parse_hex = |b, i| {
    go = |j, acc|
        match b.get(j) {
            Ok(c) =>
                if c >= 48 and c <= 57 {
                    go(j.plus(1), acc.shl_wrap(4).plus(c.minus(48).to_u64()))
                } else if c >= 65 and c <= 70 {
                    go(j.plus(1), acc.shl_wrap(4).plus(c.minus(55).to_u64()))
                } else if c >= 97 and c <= 102 {
                    go(j.plus(1), acc.shl_wrap(4).plus(c.minus(87).to_u64()))
                } else if j == i {
                    NoMatch
                } else {
                    Parsed({ value: acc, next: j })
                }

            Err(_) => if j == i { NoMatch } else { Parsed({ value: acc, next: j }) }
        }
    z : U64
    z = 0
    go(i, z)
}

# at least one decimal digit from i
parse_dec : List(U8), U64 -> [NoMatch, Parsed({ value : U64, next : U64 })]
parse_dec = |b, i| {
    go = |j, acc|
        match b.get(j) {
            Ok(c) =>
                if c >= 48 and c <= 57 {
                    go(j.plus(1), acc.shl_wrap(3).plus(acc.shl_wrap(1)).plus(c.minus(48).to_u64()))
                } else if j == i {
                    NoMatch
                } else {
                    Parsed({ value: acc, next: j })
                }

            Err(_) => if j == i { NoMatch } else { Parsed({ value: acc, next: j }) }
        }
    z : U64
    z = 0
    go(i, z)
}

hex_digit : U64 -> Str
hex_digit = |n|
    match n.bitwise_and(0x0F) {
        0 => "0"
        1 => "1"
        2 => "2"
        3 => "3"
        4 => "4"
        5 => "5"
        6 => "6"
        7 => "7"
        8 => "8"
        9 => "9"
        10 => "A"
        11 => "B"
        12 => "C"
        13 => "D"
        14 => "E"
        _ => "F"
    }

hex4 : U64 -> Str
hex4 = |v|
    "${hex_digit(v.shr_zf_wrap(12))}${hex_digit(v.shr_zf_wrap(8))}${hex_digit(v.shr_zf_wrap(4))}${hex_digit(v)}"

# `\s*\.org\s+\$HEX` — the new current address
line_org : List(U8) -> [NoMatch, Org(U64)]
line_org = |b| {
    z : U64
    z = 0
    match match_lit(b, skip_ws(b, z), ".org") {
        Matched(after) => {
            past_ws = skip_ws(b, after)
            if past_ws == after or (b.get(past_ws) ?? 0) != 36 {
                NoMatch # \s+ then '$' required
            } else {
                match parse_hex(b, past_ws.plus(1)) {
                    Parsed(p) => Org(p.value)
                    NoMatch => NoMatch
                }
            }
        }

        NoMatch => NoMatch
    }
}

# `\s*(?:(\w+):)?\s*\.ds\s+\d+` — a (possibly labeled) storage reservation
line_ds : List(U8) -> [NoMatch, Ds({ label : [None, Some(Str)], size : U64 })]
line_ds = |b| {
    z : U64
    z = 0
    start = skip_ws(b, z)
    word_end = |j| if is_word(b.get(j) ?? 0) { word_end(j.plus(1)) } else { j }
    we = word_end(start)
    labeled =
        if we > start and (b.get(we) ?? 0) == 58 {
            # "word:" — keep the label bytes, continue past the colon
            name = b.sublist({ start: start, len: we.minus(start) })
            match Str.from_utf8(name) {
                Ok(s) => { label: Some(s), pos: skip_ws(b, we.plus(1)) }
                Err(_) => { label: None, pos: start }
            }
        } else {
            { label: None, pos: start }
        }
    match match_lit(b, labeled.pos, ".ds") {
        Matched(after) => {
            past_ws = skip_ws(b, after)
            if past_ws == after {
                NoMatch # \s+ required between .ds and the count
            } else {
                match parse_dec(b, past_ws) {
                    Parsed(p) => Ds({ label: labeled.label, size: p.value })
                    NoMatch => NoMatch
                }
            }
        }

        NoMatch => NoMatch
    }
}

# fold the file's lines: `.org` sets the address, every `.ds` advances it,
# labeled `.ds` lines matching the filter emit `Label = $ADDR (size)`
labels_of : List(U8), List(Str) -> List(Str)
labels_of = |bytes, wanted| {
    step = |acc, line|
        match line_org(line) {
            Org(a) => { ..acc, addr: Some(a) }
            NoMatch =>
                match line_ds(line) {
                    Ds(d) =>
                        match acc.addr {
                            Some(a) => {
                                out =
                                    match d.label {
                                        Some(name) =>
                                            if wanted.is_empty() or wanted.contains(name) {
                                                acc.out.append("${name} = $${hex4(a)} (${d.size.to_str()})")
                                            } else {
                                                acc.out
                                            }

                                        None => acc.out
                                    }
                                { addr: Some(a.plus(d.size)), out: out }
                            }

                            None => acc
                        }

                    NoMatch => acc
                }
        }
    # split on \n, feeding each line through `step`
    z : U64
    z = 0
    scan = |acc, line_start, i|
        match bytes.get(i) {
            Ok(10) =>
                scan(step(acc, bytes.sublist({ start: line_start, len: i.minus(line_start) })), i.plus(1), i.plus(1))

            Ok(_) => scan(acc, line_start, i.plus(1))
            Err(_) =>
                step(acc, bytes.sublist({ start: line_start, len: i.minus(line_start) })).out
        }
    scan({ addr: None, out: [] }, z, z)
}

# anonymous gaps count: .ds 4 without a label shifts Foo to $0704
expect labels_of("  .org $0700\n .ds 4\nFoo: .ds 2\n".to_utf8(), []) == ["Foo = $0704 (2)"]

# label filter, whitespace tolerance (tabs), trailing comments, hex case,
# and `.org` resetting the counter
expect {
    src = "\t.org $0000\nPad_Holding:\t.ds 1 ; controller state\njunk line\n\t.org $07f0\n\t.ds 14\nA2: .ds 3\nB: .ds 1\n"
    all = labels_of(src.to_utf8(), [])
    filtered = labels_of(src.to_utf8(), ["B"])
    all == ["Pad_Holding = $0000 (1)", "A2 = $07FE (3)", "B = $0801 (1)"] and filtered == ["B = $0801 (1)"]
}

# `.ds` before any `.org` is ignored, like the Python (addr is None)
expect labels_of("Foo: .ds 2\n.org $0010\nBar: .ds 1".to_utf8(), []) == ["Bar = $0010 (1)"]

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
        [_, asm_arg, .. as label_args] => {
            bytes = Path.from_os_str(asm_arg).read_bytes!()?
            wanted = label_args.fold([], |acc, a| acc.append(Path.from_os_str(a).display()))
            z : U64
            z = 0
            print_all!(labels_of(bytes, wanted), z)
        }

        _ => Err(Usage("usage: <smb3.asm> [label ...]"))
    }
}
