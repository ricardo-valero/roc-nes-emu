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
- **Cartridge + bus**: iNES / NES 2.0 parsing, mappers 0-4, 7, 11, and 66
  (NROM, MMC1, UxROM, CNROM, MMC3 with its A12-clocked IRQ, AxROM,
  ColorDreams, GxROM — ~90% of the licensed library), CHR RAM,
  mapper-controlled mirroring with bus-conflict AND semantics on the
  discrete boards, and the NES
  CPU memory map (2 KiB RAM mirrored, live PPU registers, 8 KiB PRG RAM,
  OAM DMA, PRG at 0x8000+). Bus reads are state-returning — PPU registers
  have read side effects, and the model is honest about it. Verified against
  the [nestest](https://www.nesdev.org/wiki/Emulator_tests) golden log —
  all 8,991 instructions match (PC, registers, flags, cycles).
- **PPU (2C02)**: scanline renderer — background with loopy v/t/x scrolling,
  sprites (8×8/8×16, flips, priority, sprite-0 hit), NTSC frame timing with
  vblank/NMI — into a 256×240 palette-index framebuffer. `Nes.run_frame` +
  `Nes.framebuffer` is the frontend surface. Dot-accurate where the CPU
  can observe it: exact vblank/NMI edges, A12-driven MMC3 IRQs, and
  span-based mid-scanline rendering (`$2006` splits, hblank-anchored
  scroll movement — Battletoads plays). Known simplification: instant
  OAM DMA with a flat 513-cycle stall.
- **APU (2A03)**: all five channels — two pulses (envelope, sweep),
  triangle, noise, and DMC (sample fetches through the mapper, with CPU
  stalls) — plus the frame counter with its IRQ and $4015 status/enable.
  Mixed through the non-linear formulas into F32 at ~44.1 kHz; frontends
  drain via `Nes.take_samples`. All eight blargg
  [apu_test](https://github.com/christopherpow/nes-test-roms) ROMs pass,
  including the cycle-exact timing ones — no exclusions. Audible in both
  frontends: the browser through the audio-worklet glue, the native app
  through the roc-ray fork's PCM stream (emulation paced by the audio
  queue, wasmboy-style).
- **Play app**: a [roc-ray](https://github.com/ricardo-valero/roc-ray)
  window (our fork, which adds binary file I/O) running the emulator at
  60fps with keyboard input through the controller register ($4016) and
  runtime ROM loading.
- **Disasm**: a pure disassembler module in the core — every listing renders
  through the emulator's own verified decode table (all 256 opcodes) and
  fetches bytes through the real mapper logic, so it cannot disagree with
  the CPU. First building block of a future debugger; the pure-Roc inspect
  CLIs under `check/inspect/tools/` (disassembler, smb3 label mapper,
  scripted-input probe) are built on the same principle — no Python
  anywhere in the repo.
- **Snapshots**: the whole console as bytes and back (`Snapshot.encode` /
  `decode`) — versioned format, FNV-1a ROM identity check, ROM data
  excluded and regrafted at decode. Battery-backed PRG RAM rides the same
  module as raw version-free `.sav` bytes. Save states in both frontends;
  a pure value tree makes the encoder a plain walk, no mutable-state
  archaeology.
- Next: the dot-accurate PPU, then more mappers — see [ROADMAP.md](ROADMAP.md)
  for the full ordering and what's deliberately deferred.

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
Backspace = Select, F5 = save state (`<rom>.state`), F9 = load state,
Esc exits. Battery cartridges persist PRG RAM to `<rom>.sav`
automatically (seeded at startup, written on a ~1 s debounced dirty
check).

## Play in the browser

The web app runs on [roc-web](https://github.com/ricardo-valero/roc-web)
(same platform as roc-ngb-emu's; currently the local `../roc-web` checkout
for the pre-release battery contract — repoint at the bundle URL once
v0.4.0 is cut). The page fetches `play.nes` by default; drop any .nes
file onto the page to swap games. Battery cartridges persist their saves
in the browser (IndexedDB, keyed by ROM content hash): seeded when the
ROM loads, flushed on change and on tab-hide. F5 saves a state, F9 loads
it — one slot per ROM, surviving page reloads.

```sh
cp check/nestest/data/nestest.nes app/web/play.nes   # seed the default
roc build app/web/main.roc --output=app/web/play.wasm
roc http_server.roc -- --port 8642 --dir app/web
```

The server is pure Roc too ([basic-webserver](https://github.com/roc-lang/basic-webserver)
0.16.0): a declared file root with host-enforced MIME types and path
safety — no python in the loop. It knows nothing about the emulator: a
generic static server whose flags default to python's (`--port 8000`,
`--dir .`).

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
of the PPM). It also digests each frame's drained APU samples (`samples`
lines in the same file) — compiled and interpreted builds must agree,
which doubles as the F32 codegen parity watch. The first reference is
nestest's title menu:

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

The blargg APU suite works the same way; all eight apu_test ROMs gate
(`check/blargg-apu/passlist`, no exclusions):

```sh
roc check/blargg-apu/fetch.roc  # once
roc check/blargg-apu/main.roc -- check/blargg-apu/data/*.nes
```

The snapshot check encodes a live console mid-frame (mid-scanline in dot
time), decodes it against a freshly parsed cartridge, and steps original
and restored in lockstep — CPU state, framebuffer, and drained APU samples
must stay bit-identical every frame. Error paths (wrong ROM, unsupported
version, truncation, battery sizing) and the battery extract/inject
round-trip run against in-code synthetic cartridges. Reference runs cover
an MMC3 ROM (scanline IRQ state) and nestest:

```sh
roc check/snapshot/main.roc -- check/blargg-ppu/data/mmc3-5-MMC3.nes
roc check/snapshot/main.roc -- check/nestest/data/nestest.nes
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
