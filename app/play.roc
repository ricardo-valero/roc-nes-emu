# Play a NES ROM in a window. The ROM at rom/play.nes is embedded at build
# time (seed it once with `cp check/nestest/data/nestest.nes rom/play.nes`;
# drop any NROM game ROM there and rebuild to play it).
#
# Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select,
# Esc exits.
app [Model, program] {
    ray: platform "https://github.com/lukewilliamboswell/roc-ray/releases/download/0.9.0/3sKTYuHvxSV77dDyZrxuUYgfrAarL6ZtasWMPeH32udh.tar.zst",
    nes: "../package/main.roc",
}

import ray.App
import ray.Assets
import ray.Color
import ray.Draw
import ray.Host
import nes.Nes
import nes.Cartridge
import "../rom/play.nes" as rom : List(U8)

# Nes is boxed: passing the large nested record itself through the host's
# model round-trip crashes in the platform's refcount walk (same platform
# property roc-ngb-emu documented for its GameBoy model).
Model : {
    console : Box(Nes),
    screen : Assets.Texture,
}

scale : F32
scale = 3

program = { init!, render! }

init! : App.Init(Model, [ResourceLimit, TextureGenerationFailed, NotANesFile])
init! = App.init(
    App.default
        .with_title("roc-nes-emu")
        .with_size({ width: 768, height: 720 })
        .with_frame_pacing(Capped(60)),
    |_host| {
        screen = Assets.Texture.generate_color!({ width: 256, height: 240, color: Color.black })?
        screen.set_filter!(Point)
        screen.set_wrap!(Clamp)
        cart = Cartridge.from_bytes(rom) ? |_| NotANesFile
        Ok({ console: Box.box(Nes.from_cartridge(cart)), screen })
    },
)

render! : Model, Host, Draw.Frame => Try(Model, [Exit(I64), PixelCountMismatch, ..])
render! = |model, host, frame| {
    if host.key_pressed(KeyEscape) {
        host.exit!(0)
    }

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

    model.screen.update!(ran.framebuffer().map(nes_color))?

    frame.clear!(Color.black)
    frame.texture!({
        texture: model.screen.view(),
        source: model.screen.rect(),
        dest: { x: 0, y: 0, width: 256 * scale, height: 240 * scale },
        origin: { x: 0, y: 0 },
        rotation: 0,
        tint: Color.white,
    })

    Ok({ ..model, console: Box.box(ran) })
}

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

nes_color : U8 -> Color
nes_color = |idx| {
    i = idx.bitwise_and(0x3F).to_u64()
    base = i.shl_wrap(1).plus(i) # *3
    r = (nes_palette.get(base) ?? 0).to_u32()
    g = (nes_palette.get(base.plus(1)) ?? 0).to_u32()
    b = (nes_palette.get(base.plus(2)) ?? 0).to_u32()
    Color.from_hex_rgb(r.shl_wrap(16).bitwise_or(g.shl_wrap(8)).bitwise_or(b))
}
