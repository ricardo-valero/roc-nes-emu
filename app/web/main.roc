# Play a NES ROM in the browser on the roc-web platform.
# ROMs load at runtime: the page fetches play.nes by default; drop any .nes
# file onto the page (or use the picker) to swap games — no rebuild.
# Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select,
# F5 = save state, F9 = load state (persisted in the browser per ROM).
# NOTE: pre-release pairing — the battery and state contracts (load! taking
# sav + state bytes, push_battery!, push_state!) are in the local roc-web
# checkout; repoint at the release-bundle URL once v0.4.0 is cut and lib/
# re-vendored.
#
# Build:  roc build app/web/main.roc --output=app/web/play.wasm
# Serve:  python3 -m http.server -d app/web
app [Model, program] {
    web: platform "../../../roc-web/platform/main.roc",
    nes: "../../package/main.roc",
}

import web.App
import web.Host
import nes.Nes
import nes.Bus
import nes.Cartridge
import nes.Snapshot

Model : {
    console : Box(Nes),
    cart : Box(Cartridge), # decode target for F9 (immutable ROM data)
    frames : U64,
    last_sav : List(U8),
    state : List(U8), # latest snapshot: page-stored at init, F5 after
    f5 : Bool, # previous-frame key levels; the platform reports levels,
    f9 : Bool, # edges are the app's job
}

program = { init, render! }

init = App.init(
    App.default
        .with_title("roc-nes-emu")
        .with_screen({ width: 256, height: 240 })
        .with_scale(3)
        .with_renderer(Auto),
    # sav / state are the page's stored bytes for this cartridge (empty on
    # a clean start); battery seeding is skipped for batteryless carts and
    # wrong-sized data, state bytes are validated at F9 time
    |rom, sav, state| {
        cart =
            match Cartridge.from_bytes(rom) {
                Ok(c) => c
                Err(_) => crash("not a parseable iNES file")
            }
        fresh = Nes.from_cartridge(cart)
        console =
            if Snapshot.battery_backed(fresh) {
                Snapshot.with_battery_ram(fresh, sav) ?? fresh
            } else {
                fresh
            }
        { console: Box.box(console), cart: Box.box(cart), frames: 0, last_sav: [], state, f5: Bool.False, f9: Bool.False }
    },
)

decode_err_str = |e|
    match e {
        BadMagic => "not a snapshot"
        UnsupportedVersion(v) => "unsupported snapshot version ${v.to_str()}"
        RomMismatch => "snapshot is for a different ROM"
        Truncated => "snapshot truncated"
        _ => "snapshot corrupt"
    }

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
    # F5 rising edge: snapshot the console, keep it, persist it via the page
    f5_now = host.key_down(KeyF5)
    f9_now = host.key_down(KeyF9)
    after_save =
        if f5_now and model.f5 == Bool.False {
            match Snapshot.encode(drained.nes) {
                Ok(bytes) => {
                    host.push_state!(bytes)
                    host.log!("state saved (${bytes.len().to_str()} bytes)")
                    { nes: drained.nes, state: bytes }
                }

                Err(_) => {
                    host.log!("state save failed")
                    { nes: drained.nes, state: model.state }
                }
            }
        } else {
            { nes: drained.nes, state: model.state }
        }
    # F9 rising edge: restore the latest snapshot; a missing or rejected
    # one leaves the running console untouched
    restored =
        if f9_now and model.f9 == Bool.False {
            if after_save.state.len() == 0 {
                host.log!("no save state for this ROM")
                after_save.nes
            } else {
                match Snapshot.decode(after_save.state, Box.unbox(model.cart)) {
                    Ok(back) => {
                        host.log!("state loaded")
                        back
                    }

                    Err(e) => {
                        host.log!("state load failed: ${decode_err_str(e)}")
                        after_save.nes
                    }
                }
            }
        } else {
            after_save.nes
        }
    # Battery bytes out on a ~1 s debounced dirty check — the page retains
    # the last push for tab-hide persistence, so per-frame pushing (a full
    # save copy across the wasm boundary each frame) buys nothing. The GB
    # "RAM disabled after a write" save signal has no NES equivalent (many
    # games treat PRG RAM as scratch), so every changed push flushes.
    # Batteryless carts push nothing at all.
    last_sav =
        if Snapshot.battery_backed(restored) and model.frames.bitwise_and(63) == 63 {
            sav = Snapshot.battery_ram(restored) ?? []
            if sav != model.last_sav {
                host.push_battery!(sav, Bool.True)
                sav
            } else {
                model.last_sav
            }
        } else {
            model.last_sav
        }
    host.blit!(rgba(restored.framebuffer()))
    host.queue_audio!(interleave(drained.samples))
    { ..model,
        console: Box.box(restored),
        frames: model.frames + 1,
        last_sav,
        state: after_save.state,
        f5: f5_now,
        f9: f9_now,
    }
}

# the platform's audio sink consumes interleaved stereo at 48 kHz; the NES
# is mono, so duplicate each sample into an L/R pair
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
            p = Box.unbox(bus_state.ppu)
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
