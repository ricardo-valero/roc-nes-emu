app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    nes: "../../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import nes.Nes
import nes.Cartridge

# Headless frame capture, run from the repo root:
#
#   roc check/frame/main.roc -- <rom> <frames> [out.ppm] [<button>@<frame>[+] ...]
#
# Runs the ROM for N frames, prints the framebuffer digest (djb2 over the
# 61,440 palette indices) and the APU sample digest (djb2 over each frame's
# drained samples quantized to 16 bits), optionally writes a viewable P6
# PPM, and — if `check/frame/digests` holds lines `<rom> <frames> <digest>`
# / `<rom> <frames> samples <digest>` — compares against the frozen values
# and fails on mismatch. Digests are frozen only after the PPM has been
# visually confirmed; the sample digest doubles as the compiled-versus-
# interpreted F32 parity check (both modes must print the same value).

# djb2: multiply-free ((h << 5) + h + byte), wrapping
digest_of : List(U8) -> U64
digest_of = |bytes|
    bytes.fold(5381, |h, b| h.shl_wrap(5).plus_wrap(h).plus_wrap(b.to_u64()))

# 2C02 palette (nesdev wiki), RGB triplets by NES color index
nes_palette : List(U8)
nes_palette = [
    84, 84, 84, 0, 30, 116, 8, 16, 144, 48, 0, 136, 68, 0, 100, 92, 0, 48, 84, 4, 0, 60, 24, 0,
    32, 42, 0, 8, 58, 0, 0, 64, 0, 0, 60, 0, 0, 50, 60, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    152, 150, 152, 8, 76, 196, 48, 50, 236, 92, 30, 228, 136, 20, 176, 160, 20, 100, 152, 34, 32, 120, 60, 0,
    84, 90, 0, 40, 114, 0, 8, 124, 0, 0, 118, 40, 0, 102, 120, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    236, 238, 236, 76, 154, 236, 120, 124, 236, 176, 98, 236, 228, 84, 236, 236, 88, 180, 236, 106, 100, 212, 136, 32,
    160, 170, 0, 116, 196, 0, 76, 208, 32, 56, 204, 108, 56, 180, 204, 60, 60, 60, 0, 0, 0, 0, 0, 0,
    236, 238, 236, 168, 204, 236, 188, 188, 236, 212, 178, 236, 236, 174, 236, 236, 174, 212, 236, 180, 176, 228, 196, 144,
    204, 210, 120, 180, 222, 120, 168, 226, 144, 152, 226, 180, 160, 214, 228, 160, 162, 160, 0, 0, 0, 0, 0, 0,
]

to_ppm : List(U8) -> List(U8)
to_ppm = |fb| {
    header = "P6\n256 240\n255\n".to_utf8()
    fb.fold(header, |acc, idx| {
        base = idx.bitwise_and(0x3F).to_u64().shl_wrap(1).plus(idx.bitwise_and(0x3F).to_u64()) # *3
        r = nes_palette.get(base) ?? 0
        g = nes_palette.get(base.plus(1)) ?? 0
        b = nes_palette.get(base.plus(2)) ?? 0
        acc.append(r).append(g).append(b)
    })
}

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

# an input event: hold `button` for frames [from, from+len) — probe.roc's
# tap/hold schedule syntax, so a scene found with the probe freezes with
# the identical command line (input-dependent scenes like Punch-Out!!'s
# fight need the same taps on every run for the digest to reproduce)
Event : { button : Str, from : U64, len : U64 }

# "<name>@<frame>[+]" -> event; tap = 5 frames, `+` holds forever
parse_event : Str -> [Button(Event), Bad(Str)]
parse_event = |s| {
    bytes = s.to_utf8()
    at = bytes.fold({ i: 0, found: 0xFFFF }, |st, c| {
        next = if c == 64 and st.found == 0xFFFF { { i: st.i, found: st.i } } else { st }
        { i: next.i.plus(1), found: next.found }
    })
    if at.found == 0xFFFF {
        Bad(s)
    } else {
        name = Str.from_utf8(bytes.sublist({ start: 0, len: at.found })) ?? ""
        rest = bytes.sublist({ start: at.found.plus(1), len: bytes.len().minus(at.found).minus(1) })
        hold = (rest.last() ?? 0) == 43 # trailing '+'
        frame = parse_dec(rest, 0, 0)
        len = if hold { 100000000 } else { 5 }
        Button({ button: name, from: frame, len: len })
    }
}

buttons_at : List(Event), U64 -> { a : Bool, b : Bool, select : Bool, start : Bool, up : Bool, down : Bool, left : Bool, right : Bool }
buttons_at = |events, frame| {
    on = |name|
        events.fold(Bool.False, |acc, e|
            acc or (e.button == name and frame >= e.from and frame < e.from.plus(e.len)))
    { a: on("a"), b: on("b"), select: on("select"), start: on("start"), up: on("up"), down: on("down"), left: on("left"), right: on("right") }
}

# run frames [i, until), driving the controller from the schedule and
# draining the APU each frame, folding the samples (quantized to 16 bits)
# into a running djb2
run_frames : { nes : Nes, sdigest : U64 }, List(Event), U64, U64 -> { nes : Nes, sdigest : U64 }
run_frames = |st, events, i, until|
    if i >= until {
        st
    } else {
        stepped = Nes.run_frame(st.nes, buttons_at(events, i))
        drained = stepped.take_samples()
        h = drained.samples.fold(st.sdigest, |acc, s| {
            q = (s * 32767.0).to_u64_wrap()
            acc.shl_wrap(5).plus_wrap(acc).plus_wrap(q)
        })
        run_frames({ nes: drained.nes, sdigest: h }, events, i.plus(1), until)
    }

# find "<key> <digest>\n" in the digests file; key = "<rom> <frames>"
frozen_digest : List(U8), Str -> [None, Some(U64)]
frozen_digest = |file_bytes, key| {
    key_bytes = "${key} ".to_utf8()
    scan = |i| {
        if i >= file_bytes.len() {
            None
        } else {
            matches = key_bytes.fold({ ok: Bool.True, j: i }, |st, kb| {
                { ok: st.ok and (file_bytes.get(st.j) ?? 0) == kb, j: st.j.plus(1) }
            })
            if matches.ok {
                Some(parse_dec(file_bytes, matches.j, 0))
            } else {
                skip = |k| match file_bytes.get(k) {
                    Ok(10) => k.plus(1)
                    Ok(_) => skip(k.plus(1))
                    Err(_) => file_bytes.len()
                }
                scan(skip(i))
            }
        }
    }
    scan(0)
}

main! : List(OsStr) => Try({}, _)
main! = |args| {
    match args {
        [_, rom_arg, frames_arg, ..] => {
            rom_path = Path.from_os_str(rom_arg)
            rom_str = rom_path.display()
            frames = parse_dec(Path.from_os_str(frames_arg).display().to_utf8(), 0, 0)
            rom_bytes = rom_path.read_bytes!()?
            cart = Cartridge.from_bytes(rom_bytes) ? |_| NotANesFile
            # trailing args: `<button>@<frame>[+]` input events; the first
            # non-event arg is the optional PPM dump path
            extra = List.sublist(args, { start: 3, len: args.len() })
            sorted = extra.fold({ events: [], out: "" }, |st, arg| {
                s = Path.from_os_str(arg).display()
                match parse_event(s) {
                    Button(e) => { ..st, events: st.events.append(e) }
                    Bad(_) => if st.out == "" { { ..st, out: s } } else { st }
                }
            })
            start = { nes: Nes.from_cartridge(cart), sdigest: 5381 }
            z : U64
            z = 0
            result = run_frames(start, sorted.events, z, frames)
            done = result.nes
            fb = done.framebuffer()
            digest = digest_of(fb)
            Stdout.line!("${rom_str} after ${frames.to_str()} frames: digest ${digest.to_str()}")?
            Stdout.line!("samples digest ${result.sdigest.to_str()}")?
            written =
                if sorted.out == "" {
                    Ok({})
                } else {
                    out = Path.from_os_str(OsStr.from_str(sorted.out))
                    out.write_bytes!(to_ppm(fb))?
                    Stdout.line!("wrote ${sorted.out}")?
                    Ok({})
                }
            written?
            # frozen digest comparison
            digests = Path.from_os_str(OsStr.from_str("check/frame/digests")).read_bytes!() ?? []
            match frozen_digest(digests, "${rom_str} ${frames.to_str()}") {
                Some(expected) =>
                    if expected == digest {
                        Stdout.line!("digest matches frozen reference")
                    } else {
                        Stdout.line!("DIGEST MISMATCH: frozen ${expected.to_str()}, got ${digest.to_str()}")?
                        Err(DigestMismatch)
                    }

                None => Stdout.line!("(no frozen digest for this rom/frames pair)")
            }?
            match frozen_digest(digests, "samples ${rom_str} ${frames.to_str()}") {
                Some(expected) =>
                    if expected == result.sdigest {
                        Stdout.line!("sample digest matches frozen reference")
                    } else {
                        Stdout.line!("SAMPLE DIGEST MISMATCH: frozen ${expected.to_str()}, got ${result.sdigest.to_str()}")?
                        Err(SampleDigestMismatch)
                    }

                None => Stdout.line!("(no frozen sample digest for this rom/frames pair)")
            }
        }

        _ => Err(Usage("usage: <rom> <frames> [out.ppm] [<button>@<frame>[+] ...]"))
    }
}
