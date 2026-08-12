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
- **Cartridge + bus**: iNES / NES 2.0 parsing, mapper 0 (NROM), and the NES
  CPU memory map (2 KiB RAM mirrored, stubbed PPU/APU regions, PRG at
  0x8000+). Verified against the
  [nestest](https://www.nesdev.org/wiki/Emulator_tests) golden log —
  all 8,991 instructions match (PC, registers, flags, cycles).
- Next: the PPU.

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
