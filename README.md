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
- Next: cartridge/iNES parsing + mapper 0, then the PPU.

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
steps. The CPU single-step suite needs Tom Harte's test vectors (JSON,
gitignored — fetch once):

```sh
cd check/single-step/data
for i in $(seq 0 255); do
  f=$(printf '%02x' $i)
  curl -sL -o "$f.json" "https://raw.githubusercontent.com/SingleStepTests/65x02/main/nes6502/v1/$f.json"
done
```

Then run all opcodes (or any subset):

```sh
roc check/single-step/main.roc -- check/single-step/data/*.json
roc check/single-step/main.roc -- check/single-step/data/a9.json   # one opcode
```

Each case is executed through the pure `Cpu.step` and compared field-by-field
(registers, flags, touched memory, total cycles).

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
