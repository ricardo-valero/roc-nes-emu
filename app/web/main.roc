# Play a NES ROM in the browser on the roc-web platform.
# ROMs load at runtime: the page fetches play.nes by default; drop any .nes
# file onto the page (or use the picker) to swap games — no rebuild.
# Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select.
#
# Build:  roc build app/web/main.roc --output=app/web/play.wasm
# Serve:  python3 -m http.server -d app/web
app [Model, program] {
    web: platform "https://github.com/ricardo-valero/roc-web/releases/download/v0.3.0/4FxpZ7r4sKg5TJ7f8ZoNgWgwfNPzfhHL1Xgvz7k72xvu.tar.zst",
    nes: "../../package/main.roc",
}

import web.App
import web.Host
import nes.Nes
import nes.Bus
import nes.Cartridge

Model : { console : Box(Nes), frames : U64 }

program = { init, render! }

init = App.init(
    App.default
        .with_title("roc-nes-emu")
        .with_screen({ width: 256, height: 240 })
        .with_scale(3)
        .with_renderer(Auto),
    |rom| {
        console =
            match Cartridge.from_bytes(rom) {
                Ok(cart) => Nes.from_cartridge(cart)
                Err(_) => crash("not a parseable iNES file")
            }
        { console: Box.box(console), frames: 0 }
    },
)

render! : Model, Host => Model
render! = |model, host| {
    buttons = {
        a: host.key_down(KeyX),
        b: host.key_down(KeyZ),
        select: host.key_down(KeyBackspace),
        start: host.key_down(KeyEnter),
        up: host.key_down(KeyUp),
        down: host.key_down(KeyDown),
        left: host.key_down(KeyLeft),
        right: host.key_down(KeyRight),
    }
    ran = Box.unbox(model.console).run_frame(buttons)
    # Once a second, log the machine state the way a debugger would ask
    # for it — the fastest answer to "why are the sprites blank"
    if model.frames % 60 == 0 {
        host.log!(debug_line(ran))
    } else {
        {}
    }
    drained = ran.take_samples()
    host.blit!(rgba(drained.nes.framebuffer()))
    host.queue_audio!(drained.samples)
    { console: Box.box(drained.nes), frames: model.frames + 1 }
}

hex2 : U8 -> Str
hex2 = |v| {
    nib = |n| {
        c = n.bitwise_and(0x0F)
        if c < 10 { c.plus(48) } else { c.plus(87) }
    }
    Str.from_utf8([nib(v.shr_zf_wrap(4)), nib(v)]) ?? "??"
}

debug_line : Nes -> Str
debug_line = |n|
    match n.cpu.bus {
        Nrom(bus_state) => {
            p = bus_state.ppu
            oam = |i| {
                base = i.shl_wrap(2)
                y = p.oam.get(base) ?? 0
                t = p.oam.get(base.plus(1)) ?? 0
                x = p.oam.get(base.plus(3)) ?? 0
                "(${hex2(y)},${hex2(t)},${hex2(x)})"
            }
            i0 : U64
            i0 = 0
            "ctrl=${hex2(p.ctrl)} mask=${hex2(p.mask)} oam0..3=${oam(i0)}${oam(4)}${oam(8)}${oam(12)}"
        }

        Flat(_) => "flat bus"
    }

# NES palette indices to RGBA8 through the 2C02 palette
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

rgba : List(U8) -> List(U8)
rgba = |fb| {
    var out = List.repeat(255.U8, 245760) # 256*240*4, alpha prefilled
    var i = 0.U64
    while i < 61440 {
        idx = (fb.get(i) ?? 0).bitwise_and(0x3F).to_u64()
        base = idx.shl_wrap(1).plus(idx) # *3
        j = i.shl_wrap(2)
        out = out.set(j, nes_palette.get(base) ?? 0) ?? out
        out = out.set(j.plus(1), nes_palette.get(base.plus(1)) ?? 0) ?? out
        out = out.set(j.plus(2), nes_palette.get(base.plus(2)) ?? 0) ?? out
        i = i + 1
    }
    out
}
