app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    nes: "../../../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import nes.Nes
import nes.Cartridge
import nes.Bus
import nes.Ppu

# Headless scripted-input probe: run a ROM for N frames driving the
# controller from a schedule, dump the screen along the way, and print a
# final console-state report (PC, mapper registers, PPU state, sprite 0).
# Born from the Battletoads level-1 sprite-0 hang hunt. Run from the
# repo root:
#
#   roc check/inspect/tools/probe.roc -- <rom.nes> <frames> [event ...]
#
# Events (any number, in any order):
#   <button>@<frame>     tap: hold <button> for 5 frames starting there
#   <button>@<frame>+    hold <button> from that frame onward
#   ppm@<prefix>         write <prefix>_<frame>.ppm screen dumps
#   every@<frames>       dump cadence (default 600; needs ppm@)
#
# Buttons: a b select start up down left right. Example — tap Start
# through Battletoads' menus, then walk right, screenshotting each 300:
#
#   roc check/inspect/tools/probe.roc -- bt.nes 3000 \
#       start@150 start@300 start@600 right@1600+ ppm@/tmp/bt every@300
#
# The final report prints even without ppm@: use it to see where a game
# is stuck (wait loops read as a stable PC across runs of increasing N).

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

to_hex : U64 -> Str
to_hex = |n| {
    digit = |d| List.sublist("0123456789ABCDEF".to_utf8(), { start: d, len: 1 })
    go = |acc, v| if v == 0 { acc } else { go(digit(v.bitwise_and(0xF)).concat(acc), v.shr_zf_wrap(4)) }
    if n == 0 { "0" } else { Str.from_utf8(go([], n)) ?? "?" }
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

arg_str : OsStr -> Str
arg_str = |a| Path.from_os_str(a).display()

# an input event: hold `button` for frames [from, from+len)
Event : { button : Str, from : U64, len : U64 }

# "<name>@<frame>[+]" -> event; tap = 5 frames, `+` holds forever
parse_event : Str -> [Button(Event), Ppm(Str), Every(U64), Bad(Str)]
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
        if name == "ppm" {
            Ppm(Str.from_utf8(rest) ?? "")
        } else if name == "every" {
            Every(parse_dec(rest, 0, 0))
        } else {
            hold = (rest.last() ?? 0) == 43 # trailing '+'
            frame = parse_dec(rest, 0, 0)
            len = if hold { 100000000 } else { 5 }
            Button({ button: name, from: frame, len: len })
        }
    }
}

buttons_at : List(Event), U64 -> { a : Bool, b : Bool, select : Bool, start : Bool, up : Bool, down : Bool, left : Bool, right : Bool }
buttons_at = |events, frame| {
    on = |name|
        events.fold(Bool.False, |acc, e|
            acc or (e.button == name and frame >= e.from and frame < e.from.plus(e.len)))
    { a: on("a"), b: on("b"), select: on("select"), start: on("start"), up: on("up"), down: on("down"), left: on("left"), right: on("right") }
}

run_frames : Nes, List(Event), U64, U64 -> Nes
run_frames = |nes, events, i, until|
    if i >= until {
        nes
    } else {
        stepped = Nes.run_frame(nes, buttons_at(events, i))
        run_frames(stepped.take_samples().nes, events, i.plus(1), until)
    }

# run in dump-cadence chunks, writing <prefix>_<frame>.ppm after each
chunks! : Nes, List(Event), U64, U64, U64, Str => Try(Nes, _)
chunks! = |nes, events, i, until, every, prefix|
    if i >= until {
        Ok(nes)
    } else {
        stop = if i.plus(every) < until { i.plus(every) } else { until }
        ended = run_frames(nes, events, i, stop)
        _ = if prefix == "" {
            Ok({})
        } else {
            out = Path.from_os_str(OsStr.from_str("${prefix}_${stop.to_str()}.ppm"))
            out.write_bytes!(to_ppm(ended.framebuffer()))?
            Stdout.line!("wrote ${prefix}_${stop.to_str()}.ppm")
        }
        chunks!(ended, events, stop, until, every, prefix)
    }

mapper_str : Cartridge -> Str
mapper_str = |cart|
    match cart.mapper {
        Nrom => "NROM"
        Uxrom(m) => "UxROM bank ${to_hex(m.bank.to_u64())}"
        Cnrom(m) => "CNROM bank ${to_hex(m.bank.to_u64())}"
        Mmc1(m) => "MMC1 control ${to_hex(m.control.to_u64())} chr0 ${to_hex(m.chr0.to_u64())} chr1 ${to_hex(m.chr1.to_u64())} prg ${to_hex(m.prg_bank.to_u64())}"
        Mmc3(m) => "MMC3 select ${to_hex(m.bank_select.to_u64())} mirroring ${to_hex(m.mirroring.to_u64())} irq_enabled ${if m.irq_enabled { "yes" } else { "no" }}"
        Axrom(m) => "AxROM bank ${to_hex(m.bank.to_u64())}"
        Mmc2(m) => "MMC2 prg ${to_hex(m.prg_bank.to_u64())} chr ${to_hex(m.chr_fd0.to_u64())}/${to_hex(m.chr_fe0.to_u64())} ${to_hex(m.chr_fd1.to_u64())}/${to_hex(m.chr_fe1.to_u64())} latches ${to_hex(m.latch0.to_u64())}/${to_hex(m.latch1.to_u64())} mirroring ${to_hex(m.mirroring.to_u64())}"
        ColorDreams(m) => "ColorDreams bank ${to_hex(m.bank.to_u64())}"
        Gxrom(m) => "GxROM bank ${to_hex(m.bank.to_u64())}"
        Unsupported => "unsupported"
    }

report! : Nes => Try({}, _)
report! = |nes| {
    Stdout.line!("pc ${to_hex(nes.cpu.reg.program_counter.to_u64())} cycles ${nes.cpu.cycles.to_str()}")?
    match nes.cpu.bus {
        Nrom(n) => {
            p = Box.unbox(n.ppu)
            oam0 = [0, 1, 2, 3].fold("", |acc, k| "${acc} ${to_hex((p.oam.get(k) ?? 0).to_u64())}")
            Stdout.line!("mapper: ${mapper_str(n.cart)}")?
            Stdout.line!("ppu: ctrl ${to_hex(p.ctrl.to_u64())} mask ${to_hex(p.mask.to_u64())} status ${to_hex(p.status.to_u64())} v ${to_hex(p.v.to_u64())} t ${to_hex(p.t.to_u64())} scanline ${p.scanline.to_str()} dot ${p.dot.to_str()}")?
            Stdout.line!("oam0 (y tile attr x):${oam0}")
        }

        _ => Ok({})
    }
}

main! : List(OsStr) => Try({}, _)
main! = |args| {
    match args {
        [_, rom_arg, frames_arg, .. as rest] => {
            rom_bytes = Path.from_os_str(rom_arg).read_bytes!()?
            frames = parse_dec(arg_str(frames_arg).to_utf8(), 0, 0)
            cart = Cartridge.from_bytes(rom_bytes) ? |_| NotANesFile
            parsed = rest.fold({ events: [], ppm: "", every: 600, bad: [] }, |st, a|
                match parse_event(arg_str(a)) {
                    Button(e) => { ..st, events: st.events.append(e) }
                    Ppm(prefix) => { ..st, ppm: prefix }
                    Every(n) => { ..st, every: if n == 0 { 600 } else { n } }
                    Bad(s) => { ..st, bad: st.bad.append(s) }
                })
            if parsed.bad.len() > 0 {
                Stdout.line!("bad event (want <button>@<frame>[+], ppm@<prefix>, every@<n>): ${parsed.bad.first() ?? ""}")
            } else {
                ended = chunks!(Nes.from_cartridge(cart), parsed.events, 0, frames, parsed.every, parsed.ppm)?
                Stdout.line!("ran ${frames.to_str()} frames")?
                report!(ended)
            }
        }

        _ => Stdout.line!("usage: roc check/inspect/tools/probe.roc -- <rom.nes> <frames> [event ...]")
    }
}
