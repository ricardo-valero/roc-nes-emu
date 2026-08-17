# Roadmap

Emulation-first: this tracks the NES core and its two thin frontends
(rocray window, roc-web browser). Ordering is deliberate — notes below
say why when it isn't obvious. Specs and change history live in the
shared OpenSpec root; this file is the at-a-glance view.

## Done

- [x] **CPU** — all 256 opcodes (unofficial included, stable-behavior
  semantics), cycle accounting, page-cross penalties, interrupts.
  Gated by Tom Harte SingleStepTests (256/256) and the nestest golden
  log (8,991 lines).
- [x] **Memory bus + cartridge** — NES memory map, iNES / NES 2.0
  parsing, OAM DMA with stall accounting, controller serial port.
- [x] **PPU (scanline)** — registers with read effects, loopy
  scrolling, background + sprites, sprite-0 hit, NTSC frame timing at
  scanline granularity. Gated by frozen frame digests and the
  non-timing blargg PPU ROMs.
- [x] **APU** — all five channels including DMC with CPU stalls,
  cycle-exact frame counter. All eight blargg apu_test ROMs pass, no
  exclusions.
- [x] **Mappers 0–4** — NROM, MMC1, UxROM, CNROM, MMC3 (scanline-
  approximation IRQ). ~85% of the licensed library boots.
- [x] **Frontends** — rocray window (keyboard, F5/F9 save states,
  `.sav` persistence, PCM audio with audio-clock pacing) and roc-web
  browser (drag-and-drop ROMs, audio, battery + save states in
  IndexedDB keyed by ROM content hash).
- [x] **Save states + battery saves** — the whole console as bytes and
  back (`Snapshot`): versioned format, FNV-1a ROM identity, lockstep-
  verified round-trips; battery PRG RAM as raw version-free `.sav`.
- [x] **Performance pass** — 12.0 → 7.7 ms/frame native. The cost was
  record-copy/refcount churn in the per-instruction path, not
  rendering; fixed by fusing per-instruction bus bookkeeping and
  boxing the PPU/APU in the bus payload.

## Next

- [x] **Dot-accurate PPU, phase 1: timing skeleton** — absolute dot
  clock with lazy catch-up (the PPU materializes exact state only at
  observation points: register access, NMI/IRQ sampling). Dot-exact
  vblank/NMI edges, the NMI suppression race, the odd-frame dot skip
  with the $2001 write latency. All ten blargg ppu_vbl_nmi singles now
  gate (was three), framebuffers bit-identical, and the lazy design
  made frames *faster*: 7.7 → 7.2 ms (6.1 on nestest).
- [x] **Dot-accurate PPU, phase 2: MMC3 A12** — real A12-driven IRQ
  clocking: register-driven via $2006/$2007, fetch-driven per line via
  config-derived rise dots, low-time filter. mmc3-1..5 gate (16 blargg
  PPU ROMs total); mmc3-6 is permanently excluded — it tests the
  alternate IRQ revision, mutually exclusive with mmc3-5. IRQs land at
  true rise dots now; 6.7 ms/frame.
- [x] **Dot-accurate PPU, phase 3: mid-scanline rendering** — span-based
  rendering: register accesses flush pixels to the access dot
  (tile-granular, two-tile fetch lead), the remainder renders from the
  live fetch pointer — mid-line `$2006` scroll splits work, hblank
  writes now correctly affect the *next* line, and sprite-0 hit is
  visible at the access dot. No-split path bit-identical (every frozen
  digest held). `oam_stress` investigated and moved to the accuracy
  long tail: its failures need the per-dot sprite-evaluation timeline,
  which raster games don't. ~7.0 ms/frame.

## Later

- [x] **Mapper expansion, wave 1: 7/11/66** — AxROM (Battletoads),
  ColorDreams, GxROM: whole-window 32K PRG switching, AxROM
  single-screen select, bus-conflict AND semantics on the discrete
  boards (11/66). Purely additive — every frozen digest held,
  snapshot format still v4 (new mapper tags only).
- [x] **MMC2 (9, Punch-Out!!)** — the CHR latches flip on PPU pattern
  fetches during rendering: a fetch-observing read path (`chr_fetch`)
  threads latch state through the span renderer's fetch loops and
  commits it Bus-side like A12 pulses. No test ROM exists, so the gate
  is frozen Punch-Out!! digest scenes (the frame check harness grew
  probe-style scripted input for them) plus latch-semantics expects.
  Every prior digest held, snapshot format still v4 (tag 9 only).
  Further mappers by library coverage remain open.
- [ ] **Rewind** — a ring of snapshots; nearly free now that the
  console serializes.
- [ ] **Zapper** — light-gun input; needs framebuffer luminance
  readback at the polled dot.
- [ ] **Four Score** — 4-player controller multiplexing.
- [ ] **PAL / Dendy** — region clock ratios, APU tables, frame layout.
- [ ] **Accuracy long tail** — `ppu_open_bus` (open-bus decay),
  `oam_stress` (per-dot sprite-evaluation timeline),
  `cpu_interrupts_v2`, `instr_timing`, sprite-overflow exactness.

## Deferred, deliberately

- **Debug API** (breakpoints, memory viewers) — waiting on a better
  Roc debugging story; the pure `Disasm` module is its first building
  block.
- **roc-web v0.4.0 repoint** — both web apps pair with the local
  roc-web checkout for the battery/state contracts; repoint at the
  release bundle once it's cut and `lib/` is re-vendored.

## Inspirations

[Mesen2](https://github.com/SourMesen/Mesen2) (accuracy oracle) ·
[tetanes](https://github.com/lukexor/tetanes) (core/frontend split,
and this file's ancestor) ·
[fogleman/nes](https://github.com/fogleman/nes) (readability bar) ·
[pure-nes](https://github.com/sutajo/pure-nes) (FP proof point)
