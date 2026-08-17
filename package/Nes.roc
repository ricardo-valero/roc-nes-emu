import /Bus
import /Cpu
import /Cpu/Register
import /Cartridge

# The console: CPU + PPU (inside the bus) interleaved.
# One `step` = one CPU instruction, then the PPU catches up 3 dots per
# elapsed CPU cycle (DMA stalls folded in), then any latched NMI fires.
Nes := {
    cpu : Cpu,
    delayed_nmi : Bool,
}.{
    from_cartridge : Cartridge -> Nes
    from_cartridge = |cart| {
        cpu = Cpu.make(Register.init({}), Bus.from_cartridge(cart))
        { cpu: cpu.reset(), delayed_nmi: Bool.False }
    }

    step : Nes -> Nes
    step = |nes| {
        before = nes.cpu.cycles
        c1 = nes.cpu.step()
        # one fused bus pass: stall accounting, write-latched NMI pickup,
        # lazy PPU catch-up, mapper/APU ticking, interrupt levels
        r = c1.bus.after_step(c1.cycles - before, c1.cycles)
        c2 = { ..c1, bus: r.bus, cycles: c1.cycles.plus_wrap(r.stall) }
        # vblank-entry NMIs (mid-instruction in dot time) deliver now;
        # write-latched ones (a $2000 write enabling NMI during vblank land
        # on the instruction's last cycle - too late for this instruction's
        # interrupt sample) deliver after the NEXT instruction.
        # IRQ lines (mapper, APU frame counter, DMC) are level-triggered:
        # Cpu.irq honors the I flag, and each source's handler access
        # (mapper $E000 write, $4015 read) deasserts its line.
        after_nmi =
            if nes.delayed_nmi or r.entry_nmi {
                c2.nmi()
            } else {
                c2
            }
        final = if r.irq { after_nmi.irq() } else { after_nmi }
        { cpu: final, delayed_nmi: r.latched_nmi }
    }

    no_buttons : {} -> { a : Bool, b : Bool, select : Bool, start : Bool, up : Bool, down : Bool, left : Bool, right : Bool }
    no_buttons = |_| { a: Bool.False, b: Bool.False, select: Bool.False, start: Bool.False, up: Bool.False, down: Bool.False, left: Bool.False, right: Bool.False }

    # advance to the next vblank entry (bounded by a step budget),
    # with this frame's controller state applied first
    run_frame : Nes, { a : Bool, b : Bool, select : Bool, start : Bool, up : Bool, down : Bool, left : Bool, right : Bool } -> Nes
    run_frame = |nes0, buttons| {
        nes = { ..nes0, cpu: { ..nes0.cpu, bus: nes0.cpu.bus.set_buttons(buttons) } }
        f0 = Bus.ppu_frame(nes.cpu.bus)
        go = |n, budget| {
            if budget == 0 {
                n
            } else {
                n2 = step(n)
                if Bus.ppu_frame(n2.cpu.bus) > f0 {
                    n2
                } else {
                    go(n2, budget - 1)
                }
            }
        }
        budget : U64
        budget = 100000
        go(nes, budget)
    }

    framebuffer : Nes -> List(U8)
    framebuffer = |nes| Bus.ppu_framebuffer(nes.cpu.bus)

    # drain the APU's samples generated since the last drain (~735/frame)
    take_samples : Nes -> { nes : Nes, samples : List(F32) }
    take_samples = |nes| {
        r = nes.cpu.bus.take_samples()
        { nes: { ..nes, cpu: { ..nes.cpu, bus: r.bus } }, samples: r.samples }
    }
}

# test cartridge: reset -> 0x8000, NMI -> 0x9000 (PRG offset 0x1000)
test_rom : List(U8), List(U8) -> List(U8)
test_rom = |code, nmi_handler| {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 1, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    blank = List.repeat(0xEA, 16384)
    with_code = code.fold({ prg: blank, i: 0 }, |st, byte| {
        { prg: st.prg.set(st.i, byte) ?? st.prg, i: st.i.plus(1) }
    })
    with_nmi = nmi_handler.fold({ prg: with_code.prg, i: 0x1000 }, |st, byte| {
        { prg: st.prg.set(st.i, byte) ?? st.prg, i: st.i.plus(1) }
    })
    v1 = with_nmi.prg.set(0x3FFC, 0x00) ?? [] # reset -> 0x8000
    v2 = v1.set(0x3FFD, 0x80) ?? []
    v3 = v2.set(0x3FFA, 0x00) ?? [] # NMI -> 0x9000
    v4 = v3.set(0x3FFB, 0x90) ?? []
    header.concat(v4).concat(List.repeat(0, 8192))
}

# a $2002 poll loop observes the vblank flag set by real PPU progression
expect {
    # 8000: LDA $2002; BPL $8000; JMP $8006 (spin)
    rom = test_rom([0xAD, 0x02, 0x20, 0x10, 0xFB, 0x4C, 0x06, 0x80], [0x40])
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            go = |n, budget| {
                if budget == 0 or n.cpu.reg.program_counter == 0x8006 {
                    n
                } else {
                    go(Nes.step(n), budget - 1)
                }
            }
            budget : U64
            budget = 20000
            done = go(Nes.from_cartridge(cart), budget)
            done.cpu.reg.program_counter == 0x8006 and done.cpu.reg.accumulator.bitwise_and(0x80) != 0
        }

        Err(_) => Bool.False
    }
}

# NMI fires when enabled: the 0x9000 handler increments X once per frame
expect {
    # 8000: LDA #$80; STA $2000; JMP $8005 (spin)   9000: INX; RTI
    rom = test_rom([0xA9, 0x80, 0x8D, 0x00, 0x20, 0x4C, 0x05, 0x80], [0xE8, 0x40])
    match Cartridge.from_bytes(rom) {
        Ok(cart) => {
            # run_frame returns at NMI dispatch, before the handler's INX runs,
            # so N frames yield N-1 completed handler executions
            three_frames = Nes.from_cartridge(cart).run_frame(Nes.no_buttons({})).run_frame(Nes.no_buttons({})).run_frame(Nes.no_buttons({}))
            three_frames.cpu.reg.x >= 2
        }

        Err(_) => Bool.False
    }
}
