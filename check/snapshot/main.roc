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
import nes.Snapshot

# Snapshot round-trip check, run from the repo root:
#
#   roc check/snapshot/main.roc -- <rom> [warmup_frames] [compare_frames]
#
# Runs the ROM for `warmup_frames` (default 30), steps a few thousand
# instructions further so the snapshot lands mid-frame (mid-scanline in dot
# time), encodes, decodes against a freshly parsed cartridge, then steps the
# original and restored consoles in lockstep for `compare_frames` (default
# 10) comparing CPU state, cycle count, framebuffer, and drained APU samples
# every frame - naming the first divergent frame on mismatch. The window
# crosses NMI delivery, APU frame-counter clocks, and (on an MMC3 ROM) the
# scanline IRQ counter. Reference run: one MMC3 ROM and nestest:
#
#   roc check/snapshot/main.roc -- check/blargg-ppu/data/mmc3-5-MMC3.nes
#   roc check/snapshot/main.roc -- check/nestest/data/nestest.nes
#
# Error paths (wrong ROM, unsupported version, truncation, battery sizing)
# and the battery extract/inject round-trip run against in-code synthetic
# cartridges - no extra test data.

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

step_n : Nes, U64 -> Nes
step_n = |n, k| if k == 0 { n } else { step_n(Nes.step(n), k - 1) }

run_frames : Nes, U64 -> Nes
run_frames = |n, k|
    if k == 0 {
        n
    } else {
        run_frames(Nes.run_frame(n, Nes.no_buttons({})), k - 1)
    }

cpu_matches : Nes, Nes -> Bool
cpu_matches = |a, b|
    a.cpu.reg.program_counter == b.cpu.reg.program_counter
    and a.cpu.reg.stack_pointer == b.cpu.reg.stack_pointer
    and a.cpu.reg.accumulator == b.cpu.reg.accumulator
    and a.cpu.reg.x == b.cpu.reg.x
    and a.cpu.reg.y == b.cpu.reg.y
    and a.cpu.reg.status == b.cpu.reg.status
    and a.cpu.cycles == b.cpu.cycles

# advance both consoles one frame, drain samples, compare everything
lockstep : Nes, Nes, U64, U64 -> [Match, Diverged(U64, Str)]
lockstep = |a0, b0, frame, remaining|
    if remaining == 0 {
        Match
    } else {
        a1 = Nes.run_frame(a0, Nes.no_buttons({}))
        b1 = Nes.run_frame(b0, Nes.no_buttons({}))
        da = a1.take_samples()
        db = b1.take_samples()
        if cpu_matches(da.nes, db.nes) == Bool.False {
            Diverged(frame, "cpu state")
        } else if da.nes.framebuffer() != db.nes.framebuffer() {
            Diverged(frame, "framebuffer")
        } else if da.samples != db.samples {
            Diverged(frame, "apu samples")
        } else {
            lockstep(da.nes, db.nes, frame.plus(1), remaining - 1)
        }
    }

# 16K-PRG synthetic ROM; flags6 configures battery/mirroring
synthetic_rom : U8 -> List(U8)
synthetic_rom = |flags6| {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 1, flags6, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    prg0 = List.repeat(0xEA, 16384).set(0x3FFC, 0x00) ?? []
    prg = prg0.set(0x3FFD, 0x80) ?? []
    header.concat(prg).concat(List.repeat(0, 8192))
}

error_paths! : Nes, List(U8), Cartridge => Try({}, _)
error_paths! = |nes, snap, cart| {
    other_cart = Cartridge.from_bytes(synthetic_rom(0x00)) ? |_| NotANesFile
    wrong_rom =
        match Snapshot.decode(snap, other_cart) {
            Err(RomMismatch) => Ok({})
            _ => Err(WrongRomNotRejected)
        }
    wrong_rom?
    Stdout.line!("wrong-ROM snapshot rejected")?
    bad_ver =
        match Snapshot.decode(snap.set(4, 0xFF) ?? snap, cart) {
            Err(UnsupportedVersion(_)) => Ok({})
            _ => Err(BadVersionNotRejected)
        }
    bad_ver?
    Stdout.line!("unsupported version rejected")?
    truncated =
        match Snapshot.decode(List.sublist(snap, { start: 0, len: snap.len() - 1 }), cart) {
            Err(Truncated) => Ok({})
            Err(Corrupt) => Ok({})
            _ => Err(TruncationNotRejected)
        }
    truncated?
    Stdout.line!("truncated snapshot rejected")?
    wrong_sav =
        match Snapshot.with_battery_ram(nes, [1, 2, 3]) {
            Err(WrongSavSize) => Ok({})
            _ => Err(WrongSavSizeNotRejected)
        }
    wrong_sav?
    Stdout.line!("wrong-sized battery injection rejected")
}

battery_round_trip! : {} => Try({}, _)
battery_round_trip! = |_| {
    battery_cart = Cartridge.from_bytes(synthetic_rom(0x02)) ? |_| NotANesFile
    plain_cart = Cartridge.from_bytes(synthetic_rom(0x00)) ? |_| NotANesFile
    battery_nes = Nes.from_cartridge(battery_cart)
    plain_nes = Nes.from_cartridge(plain_cart)
    flags_ok =
        if Snapshot.battery_backed(battery_nes) and Snapshot.battery_backed(plain_nes) == Bool.False {
            Ok({})
        } else {
            Err(BatteryFlagWrong)
        }
    flags_ok?
    sav = List.repeat(0x5A, 0x2000).set(0, 0x77) ?? []
    seeded = Snapshot.with_battery_ram(battery_nes, sav)?
    visible =
        if seeded.cpu.bus.read8(0x6000).value == 0x77 {
            Ok({})
        } else {
            Err(BatterySeedNotVisible)
        }
    visible?
    out = Snapshot.battery_ram(seeded)?
    round =
        if out == sav {
            Ok({})
        } else {
            Err(BatteryRoundTripMismatch)
        }
    round?
    non_battery =
        match Snapshot.battery_ram(plain_nes) {
            Err(NotBatteryBacked) => Ok({})
            _ => Err(NonBatteryNotRejected)
        }
    non_battery?
    Stdout.line!("battery extract/inject round-trips; non-battery rejected")
}

main! : List(OsStr) => Try({}, _)
main! = |args| {
    match args {
        [_, rom_arg, ..] => {
            rom_path = Path.from_os_str(rom_arg)
            rom_str = rom_path.display()
            warmup =
                match args {
                    [_, _, w, ..] => parse_dec(Path.from_os_str(w).display().to_utf8(), 0, 0)
                    _ => 30
                }
            compare =
                match args {
                    [_, _, _, c, ..] => parse_dec(Path.from_os_str(c).display().to_utf8(), 0, 0)
                    _ => 10
                }
            rom_bytes = rom_path.read_bytes!()?
            cart = Cartridge.from_bytes(rom_bytes) ? |_| NotANesFile
            warm = run_frames(Nes.from_cartridge(cart), warmup)
            # land mid-frame: past vblank (~700 instructions), into rendering
            mid = step_n(warm, 2000)
            scanline =
                match mid.cpu.bus {
                    Bus.Nrom(n) => Box.unbox(n.ppu).scanline
                    Bus.Flat(_) => 0
                }
            # drain pending samples: the transient sample buffer is
            # deliberately not part of the snapshot
            drained = mid.take_samples()
            original = drained.nes
            snap = Snapshot.encode(original)?
            Stdout.line!("${rom_str}: snapshot at warmup ${warmup.to_str()}+2000 steps (scanline ${scanline.to_str()}), ${snap.len().to_str()} bytes")?
            fresh_cart = Cartridge.from_bytes(rom_bytes) ? |_| NotANesFile
            restored = Snapshot.decode(snap, fresh_cart)?
            immediate =
                if cpu_matches(original, restored) and original.framebuffer() == restored.framebuffer() {
                    Ok({})
                } else {
                    Err(RestoredStateDiffers)
                }
            immediate?
            result = lockstep(original, restored, 0, compare)
            verdict =
                match result {
                    Match => Stdout.line!("lockstep: ${compare.to_str()} frames bit-identical (cpu, framebuffer, samples)")
                    Diverged(frame, what) => {
                        Stdout.line!("DIVERGED at frame ${frame.to_str()}: ${what}")?
                        Err(Diverged)
                    }
                }
            verdict?
            error_paths!(original, snap, cart)?
            battery_round_trip!({})?
            Stdout.line!("snapshot check passed")
        }

        _ => Err(Usage("usage: <rom> [warmup_frames] [compare_frames]"))
    }
}
