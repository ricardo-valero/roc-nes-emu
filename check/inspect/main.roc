app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    nes: "../../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import nes.Nes
import nes.Bus
import nes.Cartridge
import nes.Ppu

# Headless console inspector: run a ROM with scripted input, dump machine
# state, write a screenshot. Born in the SMB3 invisible-sprite hunt
# (see NOTES.md); run from the repo root:
#
#   roc check/inspect/main.roc -- <rom> <frames> <out.ppm> [F:button ...]
#
# Each `F:button` presses that button for 8 frames starting at frame F,
# e.g.  1900:start 2300:right 2400:up 2500:a
# Buttons: a b select start up down left right
#
# Dumps at the end: PPUCTRL/PPUMASK, first 16 OAM entries, MMC3 state,
# and the screenshot tone-mapped through the NES palette.

Press : { at : U64, button : U8 } # button: 0=a 1=b 2=select 3=start 4=up 5=down 6=left 7=right

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

parse_press : Str -> [None, Some(Press)]
parse_press = |s| {
    bytes = s.to_utf8()
    at = parse_dec(bytes, 0, 0)
    # find the byte after ':'
    find_colon = |i|
        match bytes.get(i) {
            Ok(58) => i.plus(1)
            Ok(_) => find_colon(i.plus(1))
            Err(_) => i
        }
    z : U64
    z = 0
    name_at = find_colon(z)
    c = bytes.get(name_at) ?? 0
    c2 = bytes.get(name_at.plus(1)) ?? 0
    button =
        if c == 97 {
            Some(0) # a
        } else if c == 98 {
            Some(1) # b
        } else if c == 115 and c2 == 101 {
            Some(2) # select
        } else if c == 115 {
            Some(3) # start
        } else if c == 117 {
            Some(4) # up
        } else if c == 100 {
            Some(5) # down
        } else if c == 108 {
            Some(6) # left
        } else if c == 114 {
            Some(7) # right
        } else {
            None
        }
    match button {
        Some(bt) => Some({ at: at, button: bt })
        None => None
    }
}

buttons_for = |presses, frame| {
    none = Nes.no_buttons({})
    presses.fold(none, |acc, p| {
        if frame >= p.at and frame < p.at.plus(8) {
            match p.button {
                0 => { ..acc, a: Bool.True }
                1 => { ..acc, b: Bool.True }
                2 => { ..acc, select: Bool.True }
                3 => { ..acc, start: Bool.True }
                4 => { ..acc, up: Bool.True }
                5 => { ..acc, down: Bool.True }
                6 => { ..acc, left: Bool.True }
                _ => { ..acc, right: Bool.True }
            }
        } else {
            acc
        }
    })
}

run_frames : Nes, U64, U64, List(Press) -> Nes
run_frames = |n, frame, total, presses|
    if frame >= total {
        n
    } else {
        run_frames(Nes.run_frame(n, buttons_for(presses, frame)), frame.plus(1), total, presses)
    }

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
        base = idx.bitwise_and(0x3F).to_u64().shl_wrap(1).plus(idx.bitwise_and(0x3F).to_u64())
        r = nes_palette.get(base) ?? 0
        g = nes_palette.get(base.plus(1)) ?? 0
        b = nes_palette.get(base.plus(2)) ?? 0
        acc.append(r).append(g).append(b)
    })
}

main! : List(OsStr) => Try({}, _)
main! = |args| {
    match args {
        [_, rom_arg, frames_arg, out_arg, .. as press_args] => {
            rom_bytes = Path.from_os_str(rom_arg).read_bytes!()?
            frames = parse_dec(Path.from_os_str(frames_arg).display().to_utf8(), 0, 0)
            presses = press_args.fold([], |acc, a| {
                match parse_press(Path.from_os_str(a).display()) {
                    Some(p) => acc.append(p)
                    None => acc
                }
            })
            cart = Cartridge.from_bytes(rom_bytes) ? |_| NotANesFile
            z : U64
            z = 0
            done = run_frames(Nes.from_cartridge(cart), z, frames, presses)
            Path.from_os_str(out_arg).write_bytes!(to_ppm(done.framebuffer()))?
            match done.cpu.bus {
                Nrom(n) => {
                    p = n.ppu
                    Stdout.line!("pc=${done.cpu.reg.program_counter.to_str()} ctrl=${p.ctrl.to_str()} mask=${p.mask.to_str()} scanline=${p.scanline.to_str()}")?
                    dump! = |i|
                        if i >= 16 {
                            Ok({})
                        } else {
                            base = i.shl_wrap(2)
                            y = p.oam.get(base) ?? 0
                            t = p.oam.get(base.plus(1)) ?? 0
                            a = p.oam.get(base.plus(2)) ?? 0
                            x = p.oam.get(base.plus(3)) ?? 0
                            Stdout.line!("oam[${i.to_str()}] y=${y.to_str()} tile=${t.to_str()} attr=${a.to_str()} x=${x.to_str()}")?
                            dump!(i.plus(1))
                        }
                    dump!(z)?
                    mapper_dump =
                        match n.cart.mapper {
                            Mmc3(m) => {
                                r = |i| (m.banks.get(i) ?? 0).to_str()
                                "mmc3 sel=${m.bank_select.to_str()} R=[${r(0)} ${r(1)} ${r(2)} ${r(3)} ${r(4)} ${r(5)} ${r(6)} ${r(7)}] irq_en=${if m.irq_enabled { "1" } else { "0" }} latch=${m.irq_latch.to_str()}"
                            }

                            _ => "mapper: not mmc3"
                        }
                    Stdout.line!(mapper_dump)
                }

                Flat(_) => Stdout.line!("flat bus")
            }
        }

        _ => Err(Usage("usage: <rom> <frames> <out.ppm> [F:button ...]"))
    }
}
