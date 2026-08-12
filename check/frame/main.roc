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
#   roc check/frame/main.roc -- <rom> <frames> [out.ppm]
#
# Runs the ROM for N frames, prints the framebuffer digest (djb2 over the
# 61,440 palette indices), optionally writes a viewable P6 PPM, and — if
# `check/frame/digests` holds a line `<rom> <frames> <digest>` — compares
# against the frozen digest and fails on mismatch. Digests are frozen only
# after the PPM has been visually confirmed.

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

run_frames : Nes, U64 -> Nes
run_frames = |n, count|
    if count == 0 {
        n
    } else {
        run_frames(Nes.run_frame(n), count - 1)
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
            done = run_frames(Nes.from_cartridge(cart), frames)
            fb = done.framebuffer()
            digest = digest_of(fb)
            Stdout.line!("${rom_str} after ${frames.to_str()} frames: digest ${digest.to_str()}")?
            # optional PPM dump (4th arg)
            written =
                match args {
                    [_, _, _, out_arg, ..] => {
                        out = Path.from_os_str(out_arg)
                        out.write_bytes!(to_ppm(fb))?
                        Stdout.line!("wrote ${out.display()}")?
                        Ok({})
                    }

                    _ => Ok({})
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
            }
        }

        _ => Err(Usage("usage: <rom> <frames> [out.ppm]"))
    }
}
