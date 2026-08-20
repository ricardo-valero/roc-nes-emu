import /Bus
import /Cpu/Register
import /Cpu/Instr

# Resolved operand of one instruction
Operand : [None, Acc, Imm(U8), At(U16), Rel(U16)]

# 2A03 execution core: fetch/decode/execute as a pure state -> state step.
# Cycle accounting is accumulated in `cycles`; `jammed` models the KIL opcodes.
# Bus reads are state-returning (PPU registers have read side effects), so
# every read threads the bus through the step.
Cpu := {
    reg : Register,
    bus : Bus,
    cycles : U64,
    jammed : Bool,
    # intra-instruction access timing: the Nth bus access of an instruction
    # occupies CPU cycle N (exact for the simple ops timing ROMs probe
    # with; RMW dummy cycles deviate by one). base_cycles snapshots the
    # cycle count at instruction start; each access observes the PPU at
    # (base_cycles + N) * 3 - 2 - mid-dot of its cycle, the phase
    # calibrated against blargg's vbl_set_time/nmi_timing.
    base_cycles : U64,
    subcycle : U64,
}.{
    init : {} -> Cpu
    init = |_| {
        reg: Register.init({}),
        bus: Bus.flat(List.repeat(0, 0x10000)),
        cycles: 0,
        jammed: Bool.False,
        base_cycles: 0,
        subcycle: 0,
    }

    # Build a core from explicit state (verification harnesses)
    make : Register, Bus -> Cpu
    make = |reg, bus| {
        reg: reg,
        bus: bus,
        cycles: 0,
        jammed: Bool.False,
        base_cycles: 0,
        subcycle: 0,
    }

    # --- status flag helpers (C=0x01 Z=0x02 I=0x04 D=0x08 B=0x10 U=0x20 V=0x40 N=0x80) ---

    set_zn : U8, U8 -> U8
    set_zn = |p, v| {
        z = if v == 0 { 0x02 } else { 0x00 }
        n = v.bitwise_and(0x80)
        p.bitwise_and(0x7D).bitwise_or(z).bitwise_or(n)
    }

    set_flag : U8, U8, Bool -> U8
    set_flag = |p, mask, on|
        if on {
            p.bitwise_or(mask)
        } else {
            p.bitwise_and(mask.bitwise_not())
        }

    with_p : Cpu, U8 -> Cpu
    with_p = |cpu, p| { ..cpu, reg: cpu.reg.write8(Status, p) }

    # member is a structural copy of Register.Member8 (nested-type import bug)
    set8_zn : Cpu, [StackPointer, Accumulator, X, Y, Status], U8 -> Cpu
    set8_zn = |cpu, member, v|
        { ..cpu, reg: cpu.reg.write8(member, v).write8(Status, set_zn(cpu.reg.status, v)) }

    # --- memory / fetch helpers ---

    read8_at : Cpu, U16 -> { cpu : Cpu, value : U8 }
    read8_at = |cpu, addr| {
        n = cpu.subcycle.plus(1)
        r = cpu.bus.read8d(addr, cpu.base_cycles.plus(n) * 3 - 2)
        { cpu: { ..cpu, bus: r.bus, subcycle: n }, value: r.value }
    }

    write8_at : Cpu, U16, U8 -> Cpu
    write8_at = |cpu, addr, v| {
        n = cpu.subcycle.plus(1)
        # writes land at the end of their cycle (one dot later than reads
        # observe) - blargg 07-nmi_on_timing pins the phase
        { ..cpu, bus: cpu.bus.write8d(addr, v, cpu.base_cycles.plus(n) * 3 - 1), subcycle: n }
    }

    read16_at : Cpu, U16 -> { cpu : Cpu, value : U16 }
    read16_at = |cpu, addr| {
        lo = read8_at(cpu, addr)
        hi = read8_at(lo.cpu, addr.plus(1))
        { cpu: hi.cpu, value: hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16()) }
    }

    fetch8 : Cpu -> { cpu : Cpu, value : U8 }
    fetch8 = |cpu0| {
        r = read8_at(cpu0, cpu0.reg.program_counter)
        { cpu: { ..r.cpu, reg: r.cpu.reg.write16(ProgramCounter, r.cpu.reg.program_counter.plus_wrap(1)) }, value: r.value }
    }

    fetch16 : Cpu -> { cpu : Cpu, value : U16 }
    fetch16 = |cpu0| {
        lo = fetch8(cpu0)
        hi = fetch8(lo.cpu)
        { cpu: hi.cpu, value: hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16()) }
    }

    # 16-bit read where the high byte wraps within the page (zero-page
    # pointers and the JMP ($xxFF) hardware bug)
    read16_bug : Cpu, U16 -> { cpu : Cpu, value : U16 }
    read16_bug = |cpu0, ptr| {
        lo = read8_at(cpu0, ptr)
        hi_addr = ptr.bitwise_and(0xFF00).bitwise_or(ptr.plus_wrap(1).bitwise_and(0x00FF))
        hi = read8_at(lo.cpu, hi_addr)
        { cpu: hi.cpu, value: hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16()) }
    }

    page_crossed : U16, U16 -> Bool
    page_crossed = |a, b| a.bitwise_and(0xFF00) != b.bitwise_and(0xFF00)

    # --- stack ---

    push8 : Cpu, U8 -> Cpu
    push8 = |cpu, v| {
        addr = cpu.reg.stack_pointer.to_u16().bitwise_or(0x0100)
        pushed = write8_at(cpu, addr, v)
        { ..pushed, reg: pushed.reg.write8(StackPointer, pushed.reg.stack_pointer.minus_wrap(1)) }
    }

    pull8 : Cpu -> { cpu : Cpu, value : U8 }
    pull8 = |cpu0| {
        sp = cpu0.reg.stack_pointer.plus_wrap(1)
        addr = sp.to_u16().bitwise_or(0x0100)
        cpu = { ..cpu0, reg: cpu0.reg.write8(StackPointer, sp) }
        read8_at(cpu, addr)
    }

    # --- addressing-mode resolution ---

    resolve = |cpu0, mode|
        match mode {
            Implied => { cpu: cpu0, opd: None, crossed: Bool.False }
            Accumulator => { cpu: cpu0, opd: Acc, crossed: Bool.False }
            Immediate => {
                f = fetch8(cpu0)
                { cpu: f.cpu, opd: Imm(f.value), crossed: Bool.False }
            }

            ZeroPage => {
                f = fetch8(cpu0)
                { cpu: f.cpu, opd: At(f.value.to_u16()), crossed: Bool.False }
            }

            ZeroPageX => {
                f = fetch8(cpu0)
                { cpu: f.cpu, opd: At(f.value.plus_wrap(f.cpu.reg.x).to_u16()), crossed: Bool.False }
            }

            ZeroPageY => {
                f = fetch8(cpu0)
                { cpu: f.cpu, opd: At(f.value.plus_wrap(f.cpu.reg.y).to_u16()), crossed: Bool.False }
            }

            Absolute => {
                f = fetch16(cpu0)
                { cpu: f.cpu, opd: At(f.value), crossed: Bool.False }
            }

            AbsoluteX => {
                f = fetch16(cpu0)
                addr = f.value.plus_wrap(f.cpu.reg.x.to_u16())
                { cpu: f.cpu, opd: At(addr), crossed: page_crossed(f.value, addr) }
            }

            AbsoluteY => {
                f = fetch16(cpu0)
                addr = f.value.plus_wrap(f.cpu.reg.y.to_u16())
                { cpu: f.cpu, opd: At(addr), crossed: page_crossed(f.value, addr) }
            }

            Indirect => {
                f = fetch16(cpu0)
                r = read16_bug(f.cpu, f.value)
                { cpu: r.cpu, opd: At(r.value), crossed: Bool.False }
            }

            IndexedIndirect => {
                f = fetch8(cpu0)
                ptr = f.value.plus_wrap(f.cpu.reg.x)
                r = read16_bug(f.cpu, ptr.to_u16())
                { cpu: r.cpu, opd: At(r.value), crossed: Bool.False }
            }

            IndirectIndexed => {
                f = fetch8(cpu0)
                r = read16_bug(f.cpu, f.value.to_u16())
                addr = r.value.plus_wrap(r.cpu.reg.y.to_u16())
                { cpu: r.cpu, opd: At(addr), crossed: page_crossed(r.value, addr) }
            }

            Relative => {
                f = fetch8(cpu0)
                pc = f.cpu.reg.program_counter
                target =
                    if f.value < 0x80 {
                        pc.plus_wrap(f.value.to_u16())
                    } else {
                        pc.plus_wrap(f.value.to_u16()).minus_wrap(0x0100)
                    }
                { cpu: f.cpu, opd: Rel(target), crossed: page_crossed(pc, target) }
            }
        }

    load_val : Cpu, Operand -> { cpu : Cpu, value : U8 }
    load_val = |cpu, opd|
        match opd {
            Imm(v) => { cpu: cpu, value: v }
            At(a) => read8_at(cpu, a)
            Acc => { cpu: cpu, value: cpu.reg.accumulator }
            _ => { cpu: cpu, value: 0 }
        }

    store_val : Cpu, Operand, U8 -> Cpu
    store_val = |cpu, opd, v|
        match opd {
            At(a) => write8_at(cpu, a, v)
            Acc => { ..cpu, reg: cpu.reg.write8(Accumulator, v) }
            _ => cpu
        }

    # --- shared operation bodies ---

    adc_val : Cpu, U8 -> Cpu
    adc_val = |cpu, m| {
        a = cpu.reg.accumulator
        c = cpu.reg.status.bitwise_and(0x01)
        sum = a.to_u16().plus(m.to_u16()).plus(c.to_u16())
        r = sum.to_u8_wrap()
        p1 = set_flag(cpu.reg.status, 0x01, sum > 0xFF)
        overflow = a.bitwise_xor(m).bitwise_not().bitwise_and(a.bitwise_xor(r)).bitwise_and(0x80) != 0
        p2 = set_zn(set_flag(p1, 0x40, overflow), r)
        { ..cpu, reg: cpu.reg.write8(Accumulator, r).write8(Status, p2) }
    }

    compare_val : Cpu, U8, U8 -> Cpu
    compare_val = |cpu, r, m| {
        t = r.minus_wrap(m)
        with_p(cpu, set_flag(set_zn(cpu.reg.status, t), 0x01, r >= m))
    }

    branch_if : Cpu, Operand, Bool, Bool -> Cpu
    branch_if = |cpu, opd, cond, crossed|
        match opd {
            Rel(target) =>
                if cond {
                    extra : U64
                    extra = if crossed { 2 } else { 1 }
                    { ..cpu,
                        reg: cpu.reg.write16(ProgramCounter, target),
                        cycles: cpu.cycles.plus_wrap(extra),
                    }
                } else {
                    cpu
                }

            _ => cpu
        }

    # asl/lsr/rol/ror cores: value -> { value, carry }
    shift_left : U8, U8 -> { value : U8, carry : Bool }
    shift_left = |v, carry_in| {
        { value: v.shl_wrap(1).bitwise_or(carry_in), carry: v.bitwise_and(0x80) != 0 }
    }

    shift_right : U8, U8 -> { value : U8, carry : Bool }
    shift_right = |v, carry_in| {
        { value: v.shr_zf_wrap(1).bitwise_or(carry_in.shl_wrap(7)), carry: v.bitwise_and(0x01) != 0 }
    }

    # read-modify-write with C from the shift and ZN from the result
    rmw_shift = |cpu, opd, shifted| {
        c1 = store_val(cpu, opd, shifted.value)
        with_p(c1, set_zn(set_flag(c1.reg.status, 0x01, shifted.carry), shifted.value))
    }

    # unofficial SHA/SHX/SHY/TAS store: value = reg & (high(base)+1); on a page
    # cross the target's high byte is replaced by the stored value
    sh_write = |cpu, opd, crossed, regval|
        match opd {
            At(addr) => {
                addr_hi = addr.shr_zf_wrap(8).to_u8_wrap()
                h1 = if crossed { addr_hi } else { addr_hi.plus_wrap(1) }
                v = regval.bitwise_and(h1)
                target =
                    if crossed {
                        v.to_u16().shl_wrap(8).bitwise_or(addr.bitwise_and(0x00FF))
                    } else {
                        addr
                    }
                write8_at(cpu, target, v)
            }

            _ => cpu
        }

    # --- interrupts ---

    interrupt = |cpu0, vector, pushed_p| {
        pc = cpu0.reg.program_counter
        c1 = push8(cpu0, pc.shr_zf_wrap(8).to_u8_wrap())
        c2 = push8(c1, pc.to_u8_wrap())
        c3 = push8(c2, pushed_p)
        c4 = with_p(c3, set_flag(c3.reg.status, 0x04, Bool.True))
        r = read16_at(c4, vector)
        { ..r.cpu, reg: r.cpu.reg.write16(ProgramCounter, r.value) }
    }

    reset : Cpu -> Cpu
    reset = |cpu0| {
        r = read16_at(cpu0, 0xFFFC)
        { ..r.cpu, reg: Register.init({}).write16(ProgramCounter, r.value) }
    }

    nmi : Cpu -> Cpu
    nmi = |cpu00| {
        cpu0 = { ..cpu00, base_cycles: cpu00.cycles, subcycle: 0 }
        cpu = interrupt(cpu0, 0xFFFA, cpu0.reg.status.bitwise_and(0xEF).bitwise_or(0x20))
        { ..cpu, cycles: cpu.cycles.plus_wrap(7) }
    }

    irq : Cpu -> Cpu
    irq = |cpu00|
        if cpu00.reg.status.bitwise_and(0x04) != 0 {
            cpu00
        } else {
            cpu0 = { ..cpu00, base_cycles: cpu00.cycles, subcycle: 0 }
            cpu = interrupt(cpu0, 0xFFFE, cpu0.reg.status.bitwise_and(0xEF).bitwise_or(0x20))
            { ..cpu, cycles: cpu.cycles.plus_wrap(7) }
        }

    # --- step ---

    step : Cpu -> Cpu
    step = |cpu0|
        if cpu0.jammed {
            cpu0
        } else {
            started = { ..cpu0, base_cycles: cpu0.cycles, subcycle: 0 }
            f = read8_at(started, started.reg.program_counter)
            inst = Instr.lookup(f.value)
            cpu1 = { ..f.cpu, reg: f.cpu.reg.write16(ProgramCounter, f.cpu.reg.program_counter.plus_wrap(1)) }
            r = resolve(cpu1, Instr.mode(inst))
            pen : U64
            pen = if Instr.penalty(inst) and r.crossed { 1 } else { 0 }
            cpu2 = { ..r.cpu, cycles: r.cpu.cycles.plus_wrap(Instr.base_cycles(inst).to_u64()).plus_wrap(pen) }
            execute(cpu2, inst, r.opd, r.crossed)
        }

    # Branch and Status name overlapping but distinct flag sets, and closed tag
    # unions do not widen, so each needs its own mask.
    branch_mask : [Carry, Zero, Negative, Overflow] -> U8
    branch_mask = |flag|
        match flag {
            Carry => 0x01
            Zero => 0x02
            Overflow => 0x40
            Negative => 0x80
        }

    # Clear reaches Overflow (CLV) but Set does not (there is no SEV), so these
    # are two different closed unions and cannot share one mask function.
    clear_mask : [Carry, Decimal, InterruptDisable, Overflow] -> U8
    clear_mask = |flag|
        match flag {
            Carry => 0x01
            InterruptDisable => 0x04
            Decimal => 0x08
            Overflow => 0x40
        }

    set_mask : [Carry, Decimal, InterruptDisable] -> U8
    set_mask = |flag|
        match flag {
            Carry => 0x01
            InterruptDisable => 0x04
            Decimal => 0x08
        }

    execute : Cpu, Instr, Operand, Bool -> Cpu
    execute = |cpu, instr, opd, crossed|
        match instr {
            # loads / stores / transfers
            Load(A, _) => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, Accumulator, r.value)
            }

            Load(X, _) => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, X, r.value)
            }

            Load(Y, _) => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, Y, r.value)
            }

            Store(A, _) => store_val(cpu, opd, cpu.reg.accumulator)
            Store(X, _) => store_val(cpu, opd, cpu.reg.x)
            Store(Y, _) => store_val(cpu, opd, cpu.reg.y)
            Transfer(AtoX) => set8_zn(cpu, X, cpu.reg.accumulator)
            Transfer(AtoY) => set8_zn(cpu, Y, cpu.reg.accumulator)
            Transfer(StoX) => set8_zn(cpu, X, cpu.reg.stack_pointer)
            Transfer(XtoA) => set8_zn(cpu, Accumulator, cpu.reg.x)
            Transfer(YtoA) => set8_zn(cpu, Accumulator, cpu.reg.y)
            Transfer(XtoS) => { ..cpu, reg: cpu.reg.write8(StackPointer, cpu.reg.x) }
            # arithmetic / logic
            Alu(Adc, _) => {
                r = load_val(cpu, opd)
                adc_val(r.cpu, r.value)
            }

            Alu(Sbc, _) => {
                r = load_val(cpu, opd)
                adc_val(r.cpu, r.value.bitwise_xor(0xFF))
            }

            Alu(And, _) => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, Accumulator, r.cpu.reg.accumulator.bitwise_and(r.value))
            }

            Alu(Ora, _) => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, Accumulator, r.cpu.reg.accumulator.bitwise_or(r.value))
            }

            Alu(Eor, _) => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, Accumulator, r.cpu.reg.accumulator.bitwise_xor(r.value))
            }

            Alu(Cmp(A), _) => {
                r = load_val(cpu, opd)
                compare_val(r.cpu, r.cpu.reg.accumulator, r.value)
            }

            Alu(Cmp(X), _) => {
                r = load_val(cpu, opd)
                compare_val(r.cpu, r.cpu.reg.x, r.value)
            }

            Alu(Cmp(Y), _) => {
                r = load_val(cpu, opd)
                compare_val(r.cpu, r.cpu.reg.y, r.value)
            }

            Alu(Bit, _) => {
                r = load_val(cpu, opd)
                z = r.cpu.reg.accumulator.bitwise_and(r.value) == 0
                p = set_flag(set_flag(set_flag(r.cpu.reg.status, 0x02, z), 0x40, r.value.bitwise_and(0x40) != 0), 0x80, r.value.bitwise_and(0x80) != 0)
                with_p(r.cpu, p)
            }
            # shifts / rotates / inc / dec
            Shift(LeftArithmetic, _) => {
                r = load_val(cpu, opd)
                rmw_shift(r.cpu, opd, shift_left(r.value, 0))
            }

            Shift(RightLogical, _) => {
                r = load_val(cpu, opd)
                rmw_shift(r.cpu, opd, shift_right(r.value, 0))
            }

            Rotate(Left, _) => {
                r = load_val(cpu, opd)
                rmw_shift(r.cpu, opd, shift_left(r.value, r.cpu.reg.status.bitwise_and(0x01)))
            }

            Rotate(Right, _) => {
                r = load_val(cpu, opd)
                rmw_shift(r.cpu, opd, shift_right(r.value, r.cpu.reg.status.bitwise_and(0x01)))
            }

            Inc(Memory(_)) => {
                r = load_val(cpu, opd)
                v = r.value.plus_wrap(1)
                c1 = store_val(r.cpu, opd, v)
                with_p(c1, set_zn(c1.reg.status, v))
            }

            Dec(Memory(_)) => {
                r = load_val(cpu, opd)
                v = r.value.minus_wrap(1)
                c1 = store_val(r.cpu, opd, v)
                with_p(c1, set_zn(c1.reg.status, v))
            }

            Inc(X) => set8_zn(cpu, X, cpu.reg.x.plus_wrap(1))
            Inc(Y) => set8_zn(cpu, Y, cpu.reg.y.plus_wrap(1))
            Dec(X) => set8_zn(cpu, X, cpu.reg.x.minus_wrap(1))
            Dec(Y) => set8_zn(cpu, Y, cpu.reg.y.minus_wrap(1))
            # branches - one arm; the flag and the polarity are the whole difference
            Branch(flag, want) =>
                branch_if(cpu, opd, (cpu.reg.status.bitwise_and(branch_mask(flag)) != 0) == want, crossed)

            # jumps / subroutines / interrupts
            Jump(_) =>
                match opd {
                    At(a) => { ..cpu, reg: cpu.reg.write16(ProgramCounter, a) }
                    _ => cpu
                }

            JumpSubroutine =>
                match opd {
                    At(target) => {
                        ret = cpu.reg.program_counter.minus_wrap(1)
                        c1 = push8(cpu, ret.shr_zf_wrap(8).to_u8_wrap())
                        c2 = push8(c1, ret.to_u8_wrap())
                        { ..c2, reg: c2.reg.write16(ProgramCounter, target) }
                    }

                    _ => cpu
                }

            ReturnFrom(Subroutine) => {
                lo = pull8(cpu)
                hi = pull8(lo.cpu)
                pc = hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16()).plus_wrap(1)
                { ..hi.cpu, reg: hi.cpu.reg.write16(ProgramCounter, pc) }
            }

            Break => {
                # push the address after the padding byte
                c0 = { ..cpu, reg: cpu.reg.write16(ProgramCounter, cpu.reg.program_counter.plus_wrap(1)) }
                interrupt(c0, 0xFFFE, c0.reg.status.bitwise_or(0x30))
            }

            ReturnFrom(Interrupt) => {
                p = pull8(cpu)
                lo = pull8(p.cpu)
                hi = pull8(lo.cpu)
                pc = hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16())
                c1 = with_p(hi.cpu, p.value.bitwise_and(0xEF).bitwise_or(0x20))
                { ..c1, reg: c1.reg.write16(ProgramCounter, pc) }
            }
            # stack ops
            Push(A) => push8(cpu, cpu.reg.accumulator)
            Push(Status) => push8(cpu, cpu.reg.status.bitwise_or(0x30))
            Pull(A) => {
                r = pull8(cpu)
                set8_zn(r.cpu, Accumulator, r.value)
            }

            Pull(Status) => {
                r = pull8(cpu)
                with_p(r.cpu, r.value.bitwise_and(0xEF).bitwise_or(0x20))
            }
            # flag ops - one arm each way
            Status(Clear(flag)) => with_p(cpu, set_flag(cpu.reg.status, clear_mask(flag), Bool.False))
            Status(Set(flag)) => with_p(cpu, set_flag(cpu.reg.status, set_mask(flag), Bool.True))
            Nop => cpu

            Skip(_) => {
                r = load_val(cpu, opd) # the operand NOPs perform their read
                r.cpu
            }
            # unofficial
            Load(AandX, _) => {
                r = load_val(cpu, opd)
                c1 = set8_zn(r.cpu, Accumulator, r.value)
                { ..c1, reg: c1.reg.write8(X, r.value) }
            }

            Store(AandX, _) => store_val(cpu, opd, cpu.reg.accumulator.bitwise_and(cpu.reg.x))
            Fused(Dec, Cmp, _) => {
                r = load_val(cpu, opd)
                v = r.value.minus_wrap(1)
                c1 = store_val(r.cpu, opd, v)
                compare_val(c1, c1.reg.accumulator, v)
            }

            Fused(Inc, Sbc, _) => {
                r = load_val(cpu, opd)
                v = r.value.plus_wrap(1)
                c1 = store_val(r.cpu, opd, v)
                adc_val(c1, v.bitwise_xor(0xFF))
            }

            Fused(Shift(LeftArithmetic), Or, _) => {
                r = load_val(cpu, opd)
                s = shift_left(r.value, 0)
                c1 = store_val(r.cpu, opd, s.value)
                c2 = with_p(c1, set_flag(c1.reg.status, 0x01, s.carry))
                set8_zn(c2, Accumulator, c2.reg.accumulator.bitwise_or(s.value))
            }

            Fused(Rotate(Left), And, _) => {
                r = load_val(cpu, opd)
                s = shift_left(r.value, r.cpu.reg.status.bitwise_and(0x01))
                c1 = store_val(r.cpu, opd, s.value)
                c2 = with_p(c1, set_flag(c1.reg.status, 0x01, s.carry))
                set8_zn(c2, Accumulator, c2.reg.accumulator.bitwise_and(s.value))
            }

            Fused(Shift(RightLogical), Xor, _) => {
                r = load_val(cpu, opd)
                s = shift_right(r.value, 0)
                c1 = store_val(r.cpu, opd, s.value)
                c2 = with_p(c1, set_flag(c1.reg.status, 0x01, s.carry))
                set8_zn(c2, Accumulator, c2.reg.accumulator.bitwise_xor(s.value))
            }

            Fused(Rotate(Right), Adc, _) => {
                r = load_val(cpu, opd)
                s = shift_right(r.value, r.cpu.reg.status.bitwise_and(0x01))
                c1 = store_val(r.cpu, opd, s.value)
                c2 = with_p(c1, set_flag(c1.reg.status, 0x01, s.carry))
                adc_val(c2, s.value)
            }

            # The type admits any rmw/alu pairing; lookup emits only the six
            # above, so this is unreachable.
            Fused(_, _, _) => cpu

            Anc(_) => {
                r = load_val(cpu, opd)
                v = r.cpu.reg.accumulator.bitwise_and(r.value)
                c1 = set8_zn(r.cpu, Accumulator, v)
                with_p(c1, set_flag(c1.reg.status, 0x01, v.bitwise_and(0x80) != 0))
            }

            Alr(_) => {
                r = load_val(cpu, opd)
                v = r.cpu.reg.accumulator.bitwise_and(r.value)
                s = shift_right(v, 0)
                c1 = with_p(r.cpu, set_flag(r.cpu.reg.status, 0x01, s.carry))
                set8_zn(c1, Accumulator, s.value)
            }

            Arr(_) => {
                r0 = load_val(cpu, opd)
                v = r0.cpu.reg.accumulator.bitwise_and(r0.value)
                r = v.shr_zf_wrap(1).bitwise_or(r0.cpu.reg.status.bitwise_and(0x01).shl_wrap(7))
                p1 = set_flag(r0.cpu.reg.status, 0x01, r.bitwise_and(0x40) != 0)
                v_flag = r.bitwise_and(0x40).shr_zf_wrap(6).bitwise_xor(r.bitwise_and(0x20).shr_zf_wrap(5)) != 0
                p2 = set_zn(set_flag(p1, 0x40, v_flag), r)
                { ..r0.cpu, reg: r0.cpu.reg.write8(Accumulator, r).write8(Status, p2) }
            }

            Axs(_) => {
                r = load_val(cpu, opd)
                t = r.cpu.reg.accumulator.bitwise_and(r.cpu.reg.x)
                res = t.minus_wrap(r.value)
                c1 = with_p(r.cpu, set_flag(set_zn(r.cpu.reg.status, res), 0x01, t >= r.value))
                { ..c1, reg: c1.reg.write8(X, res) }
            }

            Xaa(_) => {
                # unstable: magic constant 0xEE (calibrated against SingleStepTests)
                r = load_val(cpu, opd)
                v = r.cpu.reg.accumulator.bitwise_or(0xEE).bitwise_and(r.cpu.reg.x).bitwise_and(r.value)
                set8_zn(r.cpu, Accumulator, v)
            }

            Lxa(_) => {
                # unstable: magic constant 0xEE (calibrated against SingleStepTests)
                r = load_val(cpu, opd)
                v = r.cpu.reg.accumulator.bitwise_or(0xEE).bitwise_and(r.value)
                c1 = set8_zn(r.cpu, Accumulator, v)
                { ..c1, reg: c1.reg.write8(X, v) }
            }

            Ahx(_) => sh_write(cpu, opd, crossed, cpu.reg.accumulator.bitwise_and(cpu.reg.x))
            Shx(_) => sh_write(cpu, opd, crossed, cpu.reg.x)
            Shy(_) => sh_write(cpu, opd, crossed, cpu.reg.y)
            Tas(_) => {
                sp = cpu.reg.accumulator.bitwise_and(cpu.reg.x)
                c1 = { ..cpu, reg: cpu.reg.write8(StackPointer, sp) }
                sh_write(c1, opd, crossed, sp)
            }

            Las(_) => {
                r = load_val(cpu, opd)
                v = r.value.bitwise_and(r.cpu.reg.stack_pointer)
                c1 = set8_zn(r.cpu, Accumulator, v)
                { ..c1, reg: c1.reg.write8(X, v).write8(StackPointer, v) }
            }

            Halt => { ..cpu, jammed: Bool.True }
        }

    # --- program helpers (used by the behavioral expects) ---

    run_until_brk : Cpu -> Cpu
    run_until_brk = |cpu0| {
        r = read8_at(cpu0, cpu0.reg.program_counter)
        if r.value == 0x00 {
            r.cpu
        } else {
            run_until_brk(step(r.cpu))
        }
    }

    boot : Cpu, List(U8) -> Cpu
    boot = |cpu0, program| {
        cpu = { ..cpu0, bus: cpu0.bus.load_flat(program) }
        run_until_brk(reset(cpu))
    }
}

# Behavioral tests carried over from the pre-consolidation experiment CPU
expect {
    cpu = Cpu.init({}).boot([0xA9, 0x05, 0x00])
    cpu.reg.accumulator == 5 and cpu.reg.status.bitwise_and(0b0000_0010) == 0 and cpu.reg.status.bitwise_and(0b1000_0000) == 0
}

expect {
    cpu = Cpu.init({}).boot([0xA9, 0x00, 0x00])
    cpu.reg.status.bitwise_and(0b0000_0010) == 0b10
}

expect {
    cpu = Cpu.init({}).boot([0xA9, 0x0A, 0xAA, 0x00])
    cpu.reg.x == 10
}

expect {
    cpu = Cpu.init({}).boot([0xA9, 0xC0, 0xAA, 0xE8, 0x00])
    cpu.reg.x == 0xC1
}

expect {
    cpu = Cpu.init({}).boot([0xA9, 0xFF, 0xAA, 0xE8, 0xE8, 0x00])
    cpu.reg.x == 1
}

expect {
    base = Cpu.init({})
    seeded = { ..base, bus: base.bus.write8(0x10, 0x55) }
    cpu = seeded.boot([0xA5, 0x10, 0x00])
    cpu.reg.accumulator == 0x55
}

# Interrupt plumbing: BRK enters the 0xFFFE handler with B set on the pushed
# status; RTI restores flow to after the padding byte
expect {
    base = Cpu.init({})
    m1 = base.bus.write16(0xFFFC, 0x8000)
    m2 = m1.write16(0xFFFE, 0x9000)
    m3 = m2.write8(0x8000, 0x00) # BRK
    m4 = m3.write8(0x9000, 0x40) # RTI
    seeded = { ..base, bus: m4 }
    cpu = seeded.reset()
    in_handler = cpu.step()
    back = in_handler.step()
    pushed_p = in_handler.bus.read8(0x01FB).value
    in_handler.reg.program_counter == 0x9000
    and in_handler.reg.status.bitwise_and(0x04) != 0
    and pushed_p.bitwise_and(0x30) == 0x30
    and back.reg.program_counter == 0x8002
    and back.reg.stack_pointer == cpu.reg.stack_pointer
}

# NMI pushes status with B clear and vectors through 0xFFFA
expect {
    base = Cpu.init({})
    m1 = base.bus.write16(0xFFFC, 0x8000)
    m2 = m1.write16(0xFFFA, 0xA000)
    seeded = { ..base, bus: m2 }
    cpu = seeded.reset()
    taken = cpu.nmi()
    pushed_p = taken.bus.read8(0x01FB).value
    taken.reg.program_counter == 0xA000 and pushed_p.bitwise_and(0x10) == 0
}
