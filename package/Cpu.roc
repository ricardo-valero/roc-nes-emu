import /Bus
import /Cpu/Register
import /Cpu/Instruction

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
}.{
    init : {} -> Cpu
    init = |_| {
        reg: Register.init({}),
        bus: Bus.flat(List.repeat(0, 0x10000)),
        cycles: 0,
        jammed: Bool.False,
    }

    # Build a core from explicit state (verification harnesses)
    make : Register, Bus -> Cpu
    make = |reg, bus| {
        reg: reg,
        bus: bus,
        cycles: 0,
        jammed: Bool.False,
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
        r = cpu.bus.read8(addr)
        { cpu: { ..cpu, bus: r.bus }, value: r.value }
    }

    read16_at : Cpu, U16 -> { cpu : Cpu, value : U16 }
    read16_at = |cpu, addr| {
        r = cpu.bus.read16(addr)
        { cpu: { ..cpu, bus: r.bus }, value: r.value }
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
        { ..cpu,
            bus: cpu.bus.write8(addr, v),
            reg: cpu.reg.write8(StackPointer, cpu.reg.stack_pointer.minus_wrap(1)),
        }
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
            At(a) => { ..cpu, bus: cpu.bus.write8(a, v) }
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
                { ..cpu, bus: cpu.bus.write8(target, v) }
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
    nmi = |cpu0| {
        cpu = interrupt(cpu0, 0xFFFA, cpu0.reg.status.bitwise_and(0xEF).bitwise_or(0x20))
        { ..cpu, cycles: cpu.cycles.plus_wrap(7) }
    }

    irq : Cpu -> Cpu
    irq = |cpu0|
        if cpu0.reg.status.bitwise_and(0x04) != 0 {
            cpu0
        } else {
            cpu = interrupt(cpu0, 0xFFFE, cpu0.reg.status.bitwise_and(0xEF).bitwise_or(0x20))
            { ..cpu, cycles: cpu.cycles.plus_wrap(7) }
        }

    # --- step ---

    step : Cpu -> Cpu
    step = |cpu0|
        if cpu0.jammed {
            cpu0
        } else {
            f = read8_at(cpu0, cpu0.reg.program_counter)
            inst = Instruction.lookup(f.value)
            cpu1 = { ..f.cpu, reg: f.cpu.reg.write16(ProgramCounter, f.cpu.reg.program_counter.plus_wrap(1)) }
            r = resolve(cpu1, inst.mode)
            pen : U64
            pen = if inst.penalty and r.crossed { 1 } else { 0 }
            cpu2 = { ..r.cpu, cycles: r.cpu.cycles.plus_wrap(inst.cycles.to_u64()).plus_wrap(pen) }
            execute(cpu2, inst.op, r.opd, r.crossed)
        }

    execute = |cpu, op, opd, crossed|
        match op {
            # loads / stores / transfers
            Lda => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, Accumulator, r.value)
            }

            Ldx => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, X, r.value)
            }

            Ldy => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, Y, r.value)
            }

            Sta => store_val(cpu, opd, cpu.reg.accumulator)
            Stx => store_val(cpu, opd, cpu.reg.x)
            Sty => store_val(cpu, opd, cpu.reg.y)
            Tax => set8_zn(cpu, X, cpu.reg.accumulator)
            Tay => set8_zn(cpu, Y, cpu.reg.accumulator)
            Tsx => set8_zn(cpu, X, cpu.reg.stack_pointer)
            Txa => set8_zn(cpu, Accumulator, cpu.reg.x)
            Tya => set8_zn(cpu, Accumulator, cpu.reg.y)
            Txs => { ..cpu, reg: cpu.reg.write8(StackPointer, cpu.reg.x) }
            # arithmetic / logic
            Adc => {
                r = load_val(cpu, opd)
                adc_val(r.cpu, r.value)
            }

            Sbc => {
                r = load_val(cpu, opd)
                adc_val(r.cpu, r.value.bitwise_xor(0xFF))
            }

            And => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, Accumulator, r.cpu.reg.accumulator.bitwise_and(r.value))
            }

            Ora => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, Accumulator, r.cpu.reg.accumulator.bitwise_or(r.value))
            }

            Eor => {
                r = load_val(cpu, opd)
                set8_zn(r.cpu, Accumulator, r.cpu.reg.accumulator.bitwise_xor(r.value))
            }

            Cmp => {
                r = load_val(cpu, opd)
                compare_val(r.cpu, r.cpu.reg.accumulator, r.value)
            }

            Cpx => {
                r = load_val(cpu, opd)
                compare_val(r.cpu, r.cpu.reg.x, r.value)
            }

            Cpy => {
                r = load_val(cpu, opd)
                compare_val(r.cpu, r.cpu.reg.y, r.value)
            }

            Bit => {
                r = load_val(cpu, opd)
                z = r.cpu.reg.accumulator.bitwise_and(r.value) == 0
                p = set_flag(set_flag(set_flag(r.cpu.reg.status, 0x02, z), 0x40, r.value.bitwise_and(0x40) != 0), 0x80, r.value.bitwise_and(0x80) != 0)
                with_p(r.cpu, p)
            }
            # shifts / rotates / inc / dec
            Asl => {
                r = load_val(cpu, opd)
                rmw_shift(r.cpu, opd, shift_left(r.value, 0))
            }

            Lsr => {
                r = load_val(cpu, opd)
                rmw_shift(r.cpu, opd, shift_right(r.value, 0))
            }

            Rol => {
                r = load_val(cpu, opd)
                rmw_shift(r.cpu, opd, shift_left(r.value, r.cpu.reg.status.bitwise_and(0x01)))
            }

            Ror => {
                r = load_val(cpu, opd)
                rmw_shift(r.cpu, opd, shift_right(r.value, r.cpu.reg.status.bitwise_and(0x01)))
            }

            Inc => {
                r = load_val(cpu, opd)
                v = r.value.plus_wrap(1)
                c1 = store_val(r.cpu, opd, v)
                with_p(c1, set_zn(c1.reg.status, v))
            }

            Dec => {
                r = load_val(cpu, opd)
                v = r.value.minus_wrap(1)
                c1 = store_val(r.cpu, opd, v)
                with_p(c1, set_zn(c1.reg.status, v))
            }

            Inx => set8_zn(cpu, X, cpu.reg.x.plus_wrap(1))
            Iny => set8_zn(cpu, Y, cpu.reg.y.plus_wrap(1))
            Dex => set8_zn(cpu, X, cpu.reg.x.minus_wrap(1))
            Dey => set8_zn(cpu, Y, cpu.reg.y.minus_wrap(1))
            # branches
            Bcc => branch_if(cpu, opd, cpu.reg.status.bitwise_and(0x01) == 0, crossed)
            Bcs => branch_if(cpu, opd, cpu.reg.status.bitwise_and(0x01) != 0, crossed)
            Bne => branch_if(cpu, opd, cpu.reg.status.bitwise_and(0x02) == 0, crossed)
            Beq => branch_if(cpu, opd, cpu.reg.status.bitwise_and(0x02) != 0, crossed)
            Bvc => branch_if(cpu, opd, cpu.reg.status.bitwise_and(0x40) == 0, crossed)
            Bvs => branch_if(cpu, opd, cpu.reg.status.bitwise_and(0x40) != 0, crossed)
            Bpl => branch_if(cpu, opd, cpu.reg.status.bitwise_and(0x80) == 0, crossed)
            Bmi => branch_if(cpu, opd, cpu.reg.status.bitwise_and(0x80) != 0, crossed)
            # jumps / subroutines / interrupts
            Jmp =>
                match opd {
                    At(a) => { ..cpu, reg: cpu.reg.write16(ProgramCounter, a) }
                    _ => cpu
                }

            Jsr =>
                match opd {
                    At(target) => {
                        ret = cpu.reg.program_counter.minus_wrap(1)
                        c1 = push8(cpu, ret.shr_zf_wrap(8).to_u8_wrap())
                        c2 = push8(c1, ret.to_u8_wrap())
                        { ..c2, reg: c2.reg.write16(ProgramCounter, target) }
                    }

                    _ => cpu
                }

            Rts => {
                lo = pull8(cpu)
                hi = pull8(lo.cpu)
                pc = hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16()).plus_wrap(1)
                { ..hi.cpu, reg: hi.cpu.reg.write16(ProgramCounter, pc) }
            }

            Brk => {
                # push the address after the padding byte
                c0 = { ..cpu, reg: cpu.reg.write16(ProgramCounter, cpu.reg.program_counter.plus_wrap(1)) }
                interrupt(c0, 0xFFFE, c0.reg.status.bitwise_or(0x30))
            }

            Rti => {
                p = pull8(cpu)
                lo = pull8(p.cpu)
                hi = pull8(lo.cpu)
                pc = hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16())
                c1 = with_p(hi.cpu, p.value.bitwise_and(0xEF).bitwise_or(0x20))
                { ..c1, reg: c1.reg.write16(ProgramCounter, pc) }
            }
            # stack ops
            Pha => push8(cpu, cpu.reg.accumulator)
            Php => push8(cpu, cpu.reg.status.bitwise_or(0x30))
            Pla => {
                r = pull8(cpu)
                set8_zn(r.cpu, Accumulator, r.value)
            }

            Plp => {
                r = pull8(cpu)
                with_p(r.cpu, r.value.bitwise_and(0xEF).bitwise_or(0x20))
            }
            # flag ops
            Clc => with_p(cpu, set_flag(cpu.reg.status, 0x01, Bool.False))
            Sec => with_p(cpu, set_flag(cpu.reg.status, 0x01, Bool.True))
            Cli => with_p(cpu, set_flag(cpu.reg.status, 0x04, Bool.False))
            Sei => with_p(cpu, set_flag(cpu.reg.status, 0x04, Bool.True))
            Cld => with_p(cpu, set_flag(cpu.reg.status, 0x08, Bool.False))
            Sed => with_p(cpu, set_flag(cpu.reg.status, 0x08, Bool.True))
            Clv => with_p(cpu, set_flag(cpu.reg.status, 0x40, Bool.False))
            Nop => {
                r = load_val(cpu, opd) # unofficial NOPs perform their operand read
                r.cpu
            }
            # unofficial
            Lax => {
                r = load_val(cpu, opd)
                c1 = set8_zn(r.cpu, Accumulator, r.value)
                { ..c1, reg: c1.reg.write8(X, r.value) }
            }

            Sax => store_val(cpu, opd, cpu.reg.accumulator.bitwise_and(cpu.reg.x))
            Dcp => {
                r = load_val(cpu, opd)
                v = r.value.minus_wrap(1)
                c1 = store_val(r.cpu, opd, v)
                compare_val(c1, c1.reg.accumulator, v)
            }

            Isc => {
                r = load_val(cpu, opd)
                v = r.value.plus_wrap(1)
                c1 = store_val(r.cpu, opd, v)
                adc_val(c1, v.bitwise_xor(0xFF))
            }

            Slo => {
                r = load_val(cpu, opd)
                s = shift_left(r.value, 0)
                c1 = store_val(r.cpu, opd, s.value)
                c2 = with_p(c1, set_flag(c1.reg.status, 0x01, s.carry))
                set8_zn(c2, Accumulator, c2.reg.accumulator.bitwise_or(s.value))
            }

            Rla => {
                r = load_val(cpu, opd)
                s = shift_left(r.value, r.cpu.reg.status.bitwise_and(0x01))
                c1 = store_val(r.cpu, opd, s.value)
                c2 = with_p(c1, set_flag(c1.reg.status, 0x01, s.carry))
                set8_zn(c2, Accumulator, c2.reg.accumulator.bitwise_and(s.value))
            }

            Sre => {
                r = load_val(cpu, opd)
                s = shift_right(r.value, 0)
                c1 = store_val(r.cpu, opd, s.value)
                c2 = with_p(c1, set_flag(c1.reg.status, 0x01, s.carry))
                set8_zn(c2, Accumulator, c2.reg.accumulator.bitwise_xor(s.value))
            }

            Rra => {
                r = load_val(cpu, opd)
                s = shift_right(r.value, r.cpu.reg.status.bitwise_and(0x01))
                c1 = store_val(r.cpu, opd, s.value)
                c2 = with_p(c1, set_flag(c1.reg.status, 0x01, s.carry))
                adc_val(c2, s.value)
            }

            Anc => {
                r = load_val(cpu, opd)
                v = r.cpu.reg.accumulator.bitwise_and(r.value)
                c1 = set8_zn(r.cpu, Accumulator, v)
                with_p(c1, set_flag(c1.reg.status, 0x01, v.bitwise_and(0x80) != 0))
            }

            Alr => {
                r = load_val(cpu, opd)
                v = r.cpu.reg.accumulator.bitwise_and(r.value)
                s = shift_right(v, 0)
                c1 = with_p(r.cpu, set_flag(r.cpu.reg.status, 0x01, s.carry))
                set8_zn(c1, Accumulator, s.value)
            }

            Arr => {
                r0 = load_val(cpu, opd)
                v = r0.cpu.reg.accumulator.bitwise_and(r0.value)
                r = v.shr_zf_wrap(1).bitwise_or(r0.cpu.reg.status.bitwise_and(0x01).shl_wrap(7))
                p1 = set_flag(r0.cpu.reg.status, 0x01, r.bitwise_and(0x40) != 0)
                v_flag = r.bitwise_and(0x40).shr_zf_wrap(6).bitwise_xor(r.bitwise_and(0x20).shr_zf_wrap(5)) != 0
                p2 = set_zn(set_flag(p1, 0x40, v_flag), r)
                { ..r0.cpu, reg: r0.cpu.reg.write8(Accumulator, r).write8(Status, p2) }
            }

            Axs => {
                r = load_val(cpu, opd)
                t = r.cpu.reg.accumulator.bitwise_and(r.cpu.reg.x)
                res = t.minus_wrap(r.value)
                c1 = with_p(r.cpu, set_flag(set_zn(r.cpu.reg.status, res), 0x01, t >= r.value))
                { ..c1, reg: c1.reg.write8(X, res) }
            }

            Xaa => {
                # unstable: magic constant 0xEE (calibrated against SingleStepTests)
                r = load_val(cpu, opd)
                v = r.cpu.reg.accumulator.bitwise_or(0xEE).bitwise_and(r.cpu.reg.x).bitwise_and(r.value)
                set8_zn(r.cpu, Accumulator, v)
            }

            Lxa => {
                # unstable: magic constant 0xEE (calibrated against SingleStepTests)
                r = load_val(cpu, opd)
                v = r.cpu.reg.accumulator.bitwise_or(0xEE).bitwise_and(r.value)
                c1 = set8_zn(r.cpu, Accumulator, v)
                { ..c1, reg: c1.reg.write8(X, v) }
            }

            Ahx => sh_write(cpu, opd, crossed, cpu.reg.accumulator.bitwise_and(cpu.reg.x))
            Shx => sh_write(cpu, opd, crossed, cpu.reg.x)
            Shy => sh_write(cpu, opd, crossed, cpu.reg.y)
            Tas => {
                sp = cpu.reg.accumulator.bitwise_and(cpu.reg.x)
                c1 = { ..cpu, reg: cpu.reg.write8(StackPointer, sp) }
                sh_write(c1, opd, crossed, sp)
            }

            Las => {
                r = load_val(cpu, opd)
                v = r.value.bitwise_and(r.cpu.reg.stack_pointer)
                c1 = set8_zn(r.cpu, Accumulator, v)
                { ..c1, reg: c1.reg.write8(X, v).write8(StackPointer, v) }
            }

            Kil => { ..cpu, jammed: Bool.True }
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
