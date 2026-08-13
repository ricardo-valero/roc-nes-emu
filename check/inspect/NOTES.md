# check/inspect — headless console inspection

Born during the SMB3 "invisible sprites" investigation (2026-08-13).
`main.roc` runs a ROM with scripted input and dumps machine state + a
screenshot; `tools/` holds the Python sidecars for reading game code.

## Example: drive SMB3 into level 1-1

```sh
roc check/inspect/main.roc -- smb3.nes 3100 /tmp/out.ppm \
    1900:start 2300:right 2400:up 2500:a
```

(World 1 map: from START the valid path is Right to the junction, then
UP to the Level 1 panel — verified byte-for-byte against the map layout
data in the captainsouthbird/smb3 disassembly.)

## Findings from the investigation

- The emulator rendered SMB3 correctly in every headless reproduction:
  title, map (navigation, bump animations, panel entry), in-level
  standing/running/scrolling, death → map round trip.
- The map tile grid decompressed into PRG-RAM at $6000 matched the
  original World1L.asm layout data byte-for-byte.
- SMB3 reference points: Pad_Holding=$17, Pad_Input=$18 (SMB3 order:
  A=$80 B=$40 Sel=$20 Start=$10 U=$08 D=$04 L=$02 R=$01),
  Map_Operation=$0729 ($0D = normal), Map_MoveRepeat=$7950 (PRG-RAM,
  $FF at rest, wraps to 0 on first held frame = move granted),
  Tile_Mem=$6000 (+$1B0 per screen, reads offset by +$110),
  VBlank_Tick=$10 (main-loop frame sync), Update_Select=$0100.
- Label addresses from the disassembly MUST be computed counting
  anonymous `.ds` gaps (see tools/asm_labels.py) — a +1/+3 drift sent
  this investigation down several wrong holes.
- Wishlist (next debug change): save-states — every probe above cost
  8–14 minutes of re-simulation from power-on.

## Resolution (2026-08-13)

The invisible sprites were a **Roc build-backend miscompilation**
(nightly-2026-08-07): a fold whose accumulator record carries an index
counter, with the body reading the counter to compute a list-write
destination, compiles to writes shifted by one (wrapped) — the counter
increments in place before the body reads it. The interpreter is
correct, which is why every headless probe (interpreted) rendered
Mario while both compiled frontends (rocray native, wasm) scrambled
OAM via `Ppu.load_oam`. Fixed by rewriting load_oam as an explicit
recursive loop. Standalone compiler repro: ~/dev/roc-repro-fold-counter;
in-repo bisect trail: repro.roc. Bonus: compiled builds run at ~61 fps —
the "slow emulator" was the interpreter.
