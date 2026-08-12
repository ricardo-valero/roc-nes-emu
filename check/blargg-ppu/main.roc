app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    nes: "../../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import nes.Nes
import nes.Cartridge

# blargg $6000 status-protocol runner, run from the repo root:
#
#   roc check/blargg-ppu/main.roc -- check/blargg-ppu/data/*.nes
#
# Protocol: $6001-$6003 = $DE $B0 $61 marks a valid test; $6000 = $80 while
# running, $81 = "press reset" (we reset once), < $80 = final result code
# (0 = pass) with zero-terminated text at $6004. ROMs listed in
# check/blargg-ppu/passlist gate (exit nonzero on regression); unlisted ROMs
# report informatively with a promotion hint.

peek : Nes, U16 -> U8
peek = |n, addr| n.cpu.bus.read8(addr).value

signature_valid : Nes -> Bool
signature_valid = |n|
    peek(n, 0x6001) == 0xDE and peek(n, 0x6002) == 0xB0 and peek(n, 0x6003) == 0x61

read_text : Nes, U16, Str -> Str
read_text = |n, addr, acc| {
    b = peek(n, addr)
    if b == 0 or addr >= 0x6104 {
        acc
    } else {
        ch = Str.from_utf8([b]) ?? "?"
        read_text(n, addr.plus_wrap(1), "${acc}${ch}")
    }
}

# step in chunks, polling the protocol; returns the result code or TimedOut
run_rom : Nes, U64, Bool -> [Finished(U8, Nes), TimedOut]
run_rom = |n, budget, did_reset| {
    if budget == 0 {
        TimedOut
    } else {
        chunk = |nn, k| if k == 0 { nn } else { chunk(Nes.step(nn), k - 1) }
        stepped = chunk(n, 2000)
        if signature_valid(stepped) {
            status = peek(stepped, 0x6000)
            if status == 0x80 {
                run_rom(stepped, budget - 1, did_reset)
            } else if status == 0x81 {
                if did_reset {
                    run_rom(stepped, budget - 1, did_reset)
                } else {
                    # "press reset": run a little longer, then reset the CPU
                    settled = chunk(stepped, 10000)
                    resetted = { cpu: settled.cpu.reset() }
                    run_rom(resetted, budget - 1, Bool.True)
                }
            } else {
                Finished(status, stepped)
            }
        } else {
            run_rom(stepped, budget - 1, did_reset)
        }
    }
}

run_file! = |arg, passlist| {
    path = Path.from_os_str(arg)
    name = path.display()
    rom_bytes = path.read_bytes!()?
    gated = passlist.contains("\n${name}\n")
    match Cartridge.from_bytes(rom_bytes) {
        Err(_) => {
            Stdout.line!("${name}: not a parseable ROM")?
            Ok(if gated { 1 } else { 0 })
        }

        Ok(cart) => {
            budget : U64
            budget = 12000 # x2000 steps
            match run_rom(Nes.from_cartridge(cart), budget, Bool.False) {
                Finished(0, _) => {
                    tag = if gated { "ok (gating)" } else { "ok (add to passlist?)" }
                    Stdout.line!("${name}: ${tag}")?
                    Ok(0)
                }

                Finished(code, done) => {
                    text = read_text(done, 0x6004, "")
                    Stdout.line!("${name}: FAIL code ${code.to_str()} - ${text}")?
                    Ok(if gated { 1 } else { 0 })
                }

                TimedOut => {
                    Stdout.line!("${name}: timed out")?
                    Ok(if gated { 1 } else { 0 })
                }
            }
        }
    }
}

run_all! = |args, idx, passlist, failed|
    match args.get(idx) {
        Err(_) => Ok(failed)
        Ok(arg) => {
            n = run_file!(arg, passlist)?
            run_all!(args, idx.plus(1), passlist, failed.plus(n))
        }
    }

main! : List(OsStr) => Try({}, _)
main! = |args| {
    passlist_bytes = Path.from_os_str(OsStr.from_str("check/blargg-ppu/passlist")).read_bytes!() ?? []
    passlist = "\n${Str.from_utf8(passlist_bytes) ?? ""}\n"
    failed = run_all!(args, 1, passlist, 0)?
    if failed > 0 {
        Stdout.line!("FAILED: ${failed.to_str()} gating ROM(s)")?
        Err(CheckFailed)
    } else {
        Stdout.line!("ok")
    }
}
