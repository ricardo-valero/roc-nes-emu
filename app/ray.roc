# Play a NES ROM in a window. The ROM is read from disk at startup — the
# first program argument (`./ray game.nes`), else rom/play.nes (seed it once
# with `cp check/nestest/data/nestest.nes rom/play.nes`) — so swapping games
# needs no rebuild.
#
# The platform is the local roc-ray fork checkout (../../roc-ray, branch
# file-io), which adds the binary read_bytes!/write_bytes! effects and PCM
# audio streaming; build its host once with `zig build` there before
# building this app. Emulation is paced by the audio queue (~60 ms depth,
# up to 4 frames per render tick).
#
# Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select,
# F5 = save state (<rom>.state), F9 = load state, Esc exits.
#
# Battery cartridges persist their 8 KiB PRG RAM to <rom>.sav: seeded at
# startup when the file exists, written back on a ~1 s debounced dirty
# check — no exit hook to miss.
app [Model, program] {
    ray: platform "../../roc-ray/platform/main.roc",
    nes: "../package/main.roc",
}

import ray.App
import ray.Assets
import ray.Audio
import ray.Color
import ray.Draw
import ray.Host
import nes.Nes
import nes.Cartridge
import nes.Snapshot

# Nes is boxed: passing the large nested record itself through the host's
# model round-trip crashes in the platform's refcount walk (same platform
# property roc-ngb-emu documented for its GameBoy model).
Model : {
    console : Box(Nes),
    cart : Box(Cartridge),
    screen : Assets.Texture,
    speaker : Audio.Stream,
    rom_path : Str,
    last_sav : List(U8), # last persisted battery bytes ([] = nothing yet)
    frame_count : U64,
    status : Str,
    status_until : U64, # frame_count deadline for the status line
}

scale : F32
scale = 3

status_frames : U64
status_frames = 180 # ~3 s

# Speaker depth emulation refills to each tick: ~60 ms at 48 kHz, on top of
# the host's device buffering (same constants as roc-ngb-emu's ray app)
target_depth : U64
target_depth = 2880

program = { init!, render! }

init! : App.Init(Model, [ResourceLimit, TextureGenerationFailed, NotANesFile])
init! = App.init(
    App.default
        .with_title("roc-nes-emu")
        .with_size({ width: 768, height: 720 })
        # 120, not 60: emulation locks to the audio clock below, and the
        # higher cap keeps queue-full ticks cheap so catch-up after a slow
        # tick isn't throttled to 16.7 ms steps
        .with_frame_pacing(Capped(120)),
    |host| {
        rom_path = host.args!().get(0) ?? "rom/play.nes"
        rom = match host.read_bytes!(rom_path) {
            Ok(bytes) => bytes
            Err(_) => crash("no ROM at ${rom_path} — copy a NES ROM there, or pass a path: ./ray game.nes")
        }
        screen = Assets.Texture.generate_color!({ width: 256, height: 240, color: Color.black })?
        screen.set_filter!(Point)
        screen.set_wrap!(Clamp)
        speaker = match Audio.create_stream!({ sample_rate: 48000, channels: 2 }) {
            Ok(stream) => stream
            Err(_) => crash("could not open a 48 kHz stereo audio stream — is an output device available?")
        }
        cart = Cartridge.from_bytes(rom) ? |_| NotANesFile
        fresh = Nes.from_cartridge(cart)
        # battery cartridges: seed PRG RAM from <rom>.sav when present
        seeded =
            if Snapshot.battery_backed(fresh) {
                match host.read_bytes!("${rom_path}.sav") {
                    Ok(sav) =>
                        match Snapshot.with_battery_ram(fresh, sav) {
                            Ok(loaded) => { console: loaded, sav: sav }
                            Err(_) => { console: fresh, sav: [] }
                        }

                    Err(_) => { console: fresh, sav: [] }
                }
            } else {
                { console: fresh, sav: [] }
            }
        Ok({
            console: Box.box(seeded.console),
            cart: Box.box(cart),
            screen,
            speaker,
            rom_path,
            last_sav: seeded.sav,
            frame_count: 0,
            status: "",
            status_until: 0,
        })
    },
)

decode_err_str = |e|
    match e {
        BadMagic => "not a snapshot file"
        UnsupportedVersion(v) => "unsupported snapshot version ${v.to_str()}"
        RomMismatch => "snapshot is for a different ROM"
        Truncated => "snapshot truncated"
        _ => "snapshot corrupt"
    }

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

    # Audio-clock pacing (wasmboy-style, mirroring app/web and ngb's ray
    # app): run emulated frames until the speaker holds ~60 ms, bounded per
    # tick. Emulation locks to the audio clock, so pacing drift shows as a
    # repeated video frame instead of an audio drop.
    var stepped = Box.unbox(model.console)
    var frames_run = 0
    var queued = model.speaker.buffered!()
    while queued < target_depth and frames_run < 4 {
        drained = stepped.run_frame(buttons).take_samples()
        stepped = drained.nes
        model.speaker.push!(interleave(drained.samples))
        queued = model.speaker.buffered!()
        frames_run = frames_run + 1
    }
    ran = stepped
    count = model.frame_count.plus(1)

    # F5: snapshot to <rom>.state
    saved =
        if host.key_pressed(KeyF5) {
            match Snapshot.encode(ran) {
                Ok(bytes) =>
                    match host.write_bytes!("${model.rom_path}.state", bytes) {
                        Ok(_) => { nes: ran, msg: "state saved" }
                        Err(_) => { nes: ran, msg: "state save failed: write error" }
                    }

                Err(_) => { nes: ran, msg: "state save failed" }
            }
        } else {
            { nes: ran, msg: "" }
        }

    # F9: restore from <rom>.state; a missing or rejected snapshot leaves
    # the running console untouched
    loaded =
        if host.key_pressed(KeyF9) {
            match host.read_bytes!("${model.rom_path}.state") {
                Ok(bytes) =>
                    match Snapshot.decode(bytes, Box.unbox(model.cart)) {
                        Ok(restored) => { nes: restored, msg: "state loaded" }
                        Err(e) => { nes: saved.nes, msg: "state load failed: ${decode_err_str(e)}" }
                    }

                Err(_) => { nes: saved.nes, msg: "no save state for this ROM" }
            }
        } else {
            saved
        }

    # battery persistence: ~1 s debounced dirty check (every 64 frames)
    persisted =
        if count.bitwise_and(63) == 0 and Snapshot.battery_backed(loaded.nes) {
            match Snapshot.battery_ram(loaded.nes) {
                Ok(sav) =>
                    if sav != model.last_sav {
                        match host.write_bytes!("${model.rom_path}.sav", sav) {
                            Ok(_) => { sav: sav, msg: loaded.msg }
                            Err(_) => { sav: model.last_sav, msg: ".sav write failed" }
                        }
                    } else {
                        { sav: model.last_sav, msg: loaded.msg }
                    }

                Err(_) => { sav: model.last_sav, msg: loaded.msg }
            }
        } else {
            { sav: model.last_sav, msg: loaded.msg }
        }

    model.screen.update!(loaded.nes.framebuffer().map(nes_color))?

    frame.clear!(Color.black)
    frame.texture!({
        texture: model.screen.view(),
        source: model.screen.rect(),
        dest: { x: 0, y: 0, width: 256 * scale, height: 240 * scale },
        origin: { x: 0, y: 0 },
        rotation: 0,
        tint: Color.white,
    })

    status = if persisted.msg == "" { model.status } else { persisted.msg }
    status_until = if persisted.msg == "" { model.status_until } else { count.plus(status_frames) }
    if status != "" and count < status_until {
        frame.debug_text!({
            pos: { x: 8, y: 8 },
            text: status,
            size: 20,
            color: Color.white,
        })
    }

    Ok({ ..model,
        console: Box.box(loaded.nes),
        last_sav: persisted.sav,
        frame_count: count,
        status,
        status_until,
    })
}

# the host stream consumes interleaved stereo; the NES is mono, so
# duplicate each sample into an L/R pair (same adaptation as app/web)
interleave : List(F32) -> List(F32)
interleave = |mono| {
    var out = List.repeat(0.0.F32, 0)
    var i = 0.U64
    while i < mono.len() {
        s = mono.get(i) ?? 0.0
        out = out.append(s).append(s)
        i = i + 1
    }
    out
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

nes_color : U8 -> Color.Rgba
nes_color = |idx| {
    i = idx.bitwise_and(0x3F).to_u64()
    base = i.shl_wrap(1).plus(i) # *3
    r = (nes_palette.get(base) ?? 0).to_u32()
    g = (nes_palette.get(base.plus(1)) ?? 0).to_u32()
    b = (nes_palette.get(base.plus(2)) ?? 0).to_u32()
    Color.from_hex_rgb(r.shl_wrap(16).bitwise_or(g.shl_wrap(8)).bitwise_or(b))
}
