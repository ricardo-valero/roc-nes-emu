# roc-nes-emu

A NES emulator written in pure [Roc](https://www.roc-lang.org), sibling to
[roc-ngb-emu](https://github.com/ricardo-valero/roc-ngb-emu). The goal: the
first *complete* purely-functional NES emulator — a pure `package/` core
(no effects, state in → state out) that native and web frontends share.

## Status

- **CPU (2A03)**: all 256 opcodes — official and unofficial — with page-cross
  cycle accounting, interrupts (reset/BRK/IRQ/NMI), and no BCD, exactly like
  the NES's 6502. Verified against Tom Harte's
  [SingleStepTests](https://github.com/SingleStepTests/65x02) (~10,000
  generated cases per opcode).
- **Cartridge + bus**: iNES / NES 2.0 parsing, mappers 0-4 (NROM, MMC1,
  UxROM, CNROM, and MMC3 with its scanline IRQ — ~85% of the licensed
  library), CHR RAM, mapper-controlled mirroring, and the NES
  CPU memory map (2 KiB RAM mirrored, live PPU registers, 8 KiB PRG RAM,
  OAM DMA, PRG at 0x8000+). Bus reads are state-returning — PPU registers
  have read side effects, and the model is honest about it. Verified against
  the [nestest](https://www.nesdev.org/wiki/Emulator_tests) golden log —
  all 8,991 instructions match (PC, registers, flags, cycles).
- **PPU (2C02)**: scanline renderer — background with loopy v/t/x scrolling,
  sprites (8×8/8×16, flips, priority, sprite-0 hit), NTSC frame timing with
  vblank/NMI — into a 256×240 palette-index framebuffer. `Nes.run_frame` +
  `Nes.framebuffer` is the frontend surface. Known simplifications:
  scanline granularity (no mid-scanline raster effects), instant OAM DMA
  with a flat 513-cycle stall.
- **Play app**: a [roc-ray](https://github.com/ricardo-valero/roc-ray)
  window (our fork, which adds binary file I/O) running the emulator at
  60fps with keyboard input through the controller register ($4016) and
  runtime ROM loading.
- Next: the APU, more mappers, the web platform.

## Play

The ROM is read from disk at startup — the first argument, else
`rom/play.nes` — so swapping games needs no rebuild. The platform is the
local [roc-ray fork](https://github.com/ricardo-valero/roc-ray) checkout
at `../roc-ray` (branch `file-io`); build its host once with `zig build`
there, then (inside `nix develop`):

```sh
cp check/nestest/data/nestest.nes rom/play.nes   # seed the default once
roc build app/ray.roc --output=ray
./ray your-game.nes                              # any NROM ROM, no rebuild
./ray                                            # plays rom/play.nes
```

Controls: arrows = d-pad, X = A, Z = B, Enter = Start,
Backspace = Select, Esc exits.

## Development

The Nix devshell provides the Zig-based Roc compiler (pinned nightly via
[roc-overlay](https://github.com/roc-lang/roc-overlay)) and `nixd`:

```sh
nix develop          # or direnv allow
roc check package/main.roc
roc test package/main.roc
```

The pre-migration 2024 codebase (old Rust-compiler Roc syntax) lives on the
`legacy` branch.

## Checks

Verification harnesses are pure Roc programs under `check/` — no nix build
steps, and no curl: even the test-data fetch is a Roc app using basic-cli's
HTTP client. The CPU single-step suite needs Tom Harte's test vectors
(~860 MB of JSON, gitignored — fetch once, resumes if interrupted):

```sh
roc check/single-step/fetch.roc
```

Then run all opcodes (or any subset):

```sh
roc check/single-step/main.roc -- check/single-step/data/*.json
roc check/single-step/main.roc -- check/single-step/data/a9.json   # one opcode
```

Each case is executed through the pure `Cpu.step` and compared field-by-field
(registers, flags, touched memory, total cycles).

The nestest check boots kevtris' test cartridge in automation mode and
diffs every instruction against the canonical golden log (ROM from
[nes-test-roms](https://github.com/christopherpow/nes-test-roms), log from
the canonical qmtpro copy — both fetched, gitignored):

```sh
roc check/nestest/fetch.roc     # once
roc check/nestest/main.roc
```

The frame check renders a ROM headlessly and holds the framebuffer to a
frozen digest (`check/frame/digests`, frozen only after visual confirmation
of the PPM). The first reference is nestest's title menu:

```sh
roc check/frame/main.roc -- check/nestest/data/nestest.nes 60 /tmp/frame.ppm
```

The blargg PPU suite runs the self-reporting test ROMs through their $6000
status protocol; `check/blargg-ppu/passlist` records which ROMs gate (and
the exclusions, with reasons):

```sh
roc check/blargg-ppu/fetch.roc  # once
roc check/blargg-ppu/main.roc -- check/blargg-ppu/data/*.nes
```

## Inspirations

- [Mesen2](https://github.com/SourMesen/Mesen2) — the accuracy oracle
- [tetanes](https://github.com/lukexor/tetanes) (Rust) — core/frontend split
- [fogleman/nes](https://github.com/fogleman/nes) (Go) — the readability bar
- [nez](https://github.com/srijan-paul/nez) (Zig + raylib) — structural cousin
- [jsnes](https://github.com/bfirsh/jsnes) (JavaScript) — the web target
- [pure-nes](https://github.com/sutajo/pure-nes) (Haskell) — closest finished FP NES emulator
- [elmo](https://github.com/gsamokovarov/elmo) (Elm) — the trail that stopped at the CPU

Reference documentation: the [NESdev wiki](https://www.nesdev.org/wiki/),
[unofficial opcodes](https://www.nesdev.org/wiki/CPU_unofficial_opcodes), and
[TASVideos accuracy tests](https://tasvideos.org/EmulatorResources/NESAccuracyTests).
