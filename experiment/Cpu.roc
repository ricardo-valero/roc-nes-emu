import /Memory
import /Register

AddressingMode : [
    Immediate,
    Absolute([None, X, Y]),
    ZeroPage([None, X, Y]),
    IndexedIndirect([X]),
    IndirectIndexed([Y]),
]

Cpu := {
    register : Register.Type,
    memory : Memory.Type,
}.{
    get_operand_address : Cpu, AddressingMode -> U16
    get_operand_address = |cpu, mode| {
        program_counter = Register.read16(cpu.register, ProgramCounter)
        match mode {
            Immediate => program_counter
            Absolute(m) => {
                offset =
                    match m {
                        X => Register.read8(cpu.register, X)
                        Y => Register.read8(cpu.register, Y)
                        _ => 0
                    }
                base = Memory.read16(cpu.memory, program_counter)
                base.plus_wrap(offset.to_u16())
            }

            ZeroPage(m) => {
                offset =
                    match m {
                        X => Register.read8(cpu.register, X)
                        Y => Register.read8(cpu.register, Y)
                        _ => 0
                    }
                pos = Memory.read8(cpu.memory, program_counter)
                pos.plus_wrap(offset).to_u16()
            }

            IndexedIndirect(m) => {
                offset =
                    match m {
                        X => Register.read8(cpu.register, X)
                    }
                base = Memory.read8(cpu.memory, program_counter)
                ptr = base.plus_wrap(offset)
                lo = Memory.read8(cpu.memory, ptr.to_u16())
                hi = Memory.read8(cpu.memory, ptr.plus_wrap(1).to_u16())
                hi.to_u16().shl_wrap(8).bitwise_or(lo.to_u16())
            }

            IndirectIndexed(m) => {
                offset =
                    match m {
                        Y => Register.read8(cpu.register, Y)
                    }
                base = Memory.read8(cpu.memory, program_counter)
                lo = Memory.read8(cpu.memory, base.to_u16())
                hi = Memory.read8(cpu.memory, base.plus_wrap(1).to_u16())
                deref_base = hi.to_u16().shl_wrap(8).bitwise_or(lo.to_u16())
                deref = deref_base.plus_wrap(offset.to_u16())
                deref
            }
        }
    }

    # `or`/`and`/`xor` are operator keywords in the new syntax; renamed op_*
    op_or : Cpu, AddressingMode -> Cpu
    op_or = |cpu, mode| {
        addr = get_operand_address(cpu, mode)
        value = Memory.read8(cpu.memory, addr)
        result = value.bitwise_or(Register.read8(cpu.register, Accumulator))
        { ..cpu, register: Register.write8(cpu.register, Accumulator, result) }
    }

    op_and : Cpu, AddressingMode -> Cpu
    op_and = |cpu, mode| {
        addr = get_operand_address(cpu, mode)
        value = Memory.read8(cpu.memory, addr)
        result = value.bitwise_and(Register.read8(cpu.register, Accumulator))
        { ..cpu, register: Register.write8(cpu.register, Accumulator, result) }
    }

    op_xor : Cpu, AddressingMode -> Cpu
    op_xor = |cpu, mode| {
        addr = get_operand_address(cpu, mode)
        value = Memory.read8(cpu.memory, addr)
        result = value.bitwise_xor(Register.read8(cpu.register, Accumulator))
        { ..cpu, register: Register.write8(cpu.register, Accumulator, result) }
    }

    # sta, stx, sty
    st : Cpu, Register.Member8, AddressingMode -> Cpu
    st = |cpu, m, mode| {
        addr = get_operand_address(cpu, mode)
        mem = Memory.write8(cpu.memory, addr, Register.read8(cpu.register, m))
        { ..cpu, memory: mem }
    }

    # lda, ldx, ldy
    ld : Cpu, Register.Member8, AddressingMode -> Cpu
    ld = |cpu, m, mode| {
        addr = get_operand_address(cpu, mode)
        data = Memory.read8(cpu.memory, addr)
        reg = Register.write8(cpu.register, m, data)
        { ..cpu, register: update_flags(reg, data) }
    }

    # tax, tay
    ta : Cpu, Register.Member8 -> Cpu
    ta = |cpu, m| {
        data = cpu.register.accumulator
        reg = Register.write8(cpu.register, m, data)
        { ..cpu, register: update_flags(reg, data) }
    }

    inc : Cpu, AddressingMode -> Cpu
    inc = |cpu, mode| {
        addr = get_operand_address(cpu, mode)
        data = Memory.read8(cpu.memory, addr)
        result = data.plus_wrap(1)
        { ..cpu,
            memory: Memory.write8(cpu.memory, addr, result),
            register: update_flags(cpu.register, result),
        }
    }

    dec : Cpu, AddressingMode -> Cpu
    dec = |cpu, mode| {
        addr = get_operand_address(cpu, mode)
        data = Memory.read8(cpu.memory, addr)
        result = data.minus_wrap(1)
        { ..cpu,
            memory: Memory.write8(cpu.memory, addr, result),
            register: update_flags(cpu.register, result),
        }
    }

    # inx, iny (`in` is a reserved word in the new syntax; renamed inr)
    inr : Cpu, Register.Member8 -> Cpu
    inr = |cpu, m| {
        data = Register.read8(cpu.register, m)
        result = data.plus_wrap(1)
        reg = Register.write8(cpu.register, m, result)
        { ..cpu, register: update_flags(reg, result) }
    }

    # dex, dey
    de : Cpu, Register.Member8 -> Cpu
    de = |cpu, m| {
        data = Register.read8(cpu.register, m)
        result = data.minus_wrap(1)
        reg = Register.write8(cpu.register, m, result)
        { ..cpu, register: update_flags(reg, result) }
    }

    update_flags : Register.Type, U8 -> Register.Type
    update_flags = |reg, result| {
        new_status = if result == 0 { reg.status.bitwise_or(0b0000_0010) } else { reg.status.bitwise_and(0b1111_1101) }
        new_status2 = if result.bitwise_and(0b1000_0000) != 0 { new_status.bitwise_or(0b1000_0000) } else { new_status.bitwise_and(0b0111_1111) }
        Register.write8(reg, Status, new_status2)
    }

    count = |c| { ..c, register: Register.write16(c.register, ProgramCounter, Register.read16(c.register, ProgramCounter).plus_wrap(1)) }

    run : Cpu -> Cpu
    run = |cpu| {
        code = Memory.read8(cpu.memory, Register.read16(cpu.register, ProgramCounter))
        current = count(cpu)
        match code {
            0x09 => current.op_or(Immediate).run()
            0x0D => current.op_or(Absolute(None)).run()
            0x1D => current.op_or(Absolute(X)).run()
            0x19 => current.op_or(Absolute(Y)).run()
            0x05 => current.op_or(ZeroPage(None)).run()
            0x15 => current.op_or(ZeroPage(X)).run()
            0x01 => current.op_or(IndexedIndirect(X)).run()
            0x11 => current.op_or(IndirectIndexed(Y)).run()
            0x29 => current.op_and(Immediate).run()
            0x2D => current.op_and(Absolute(None)).run()
            0x3D => current.op_and(Absolute(X)).run()
            0x39 => current.op_and(Absolute(Y)).run()
            0x25 => current.op_and(ZeroPage(None)).run()
            0x35 => current.op_and(ZeroPage(X)).run()
            0x21 => current.op_and(IndexedIndirect(X)).run()
            0x31 => current.op_and(IndirectIndexed(Y)).run()
            0x49 => current.op_xor(Immediate).run()
            0x4D => current.op_xor(Absolute(None)).run()
            0x5D => current.op_xor(Absolute(X)).run()
            0x59 => current.op_xor(Absolute(Y)).run()
            0x45 => current.op_xor(ZeroPage(None)).run()
            0x55 => current.op_xor(ZeroPage(X)).run()
            0x41 => current.op_xor(IndexedIndirect(X)).run()
            0x51 => current.op_xor(IndirectIndexed(Y)).run()
            0xE8 => current.inr(X).run()
            0xC8 => current.inr(Y).run()
            0xE6 => current.inc(ZeroPage(None)).run()
            0xEE => current.inc(Absolute(None)).run()
            0xF6 => current.inc(ZeroPage(X)).run()
            0xFE => current.inc(Absolute(X)).run()
            0xCA => current.de(X).run()
            0x88 => current.de(Y).run()
            0xC6 => current.dec(ZeroPage(None)).run()
            0xCE => current.dec(Absolute(None)).run()
            0xD6 => current.dec(ZeroPage(X)).run()
            0xDE => current.dec(Absolute(X)).run()
            0xA9 => current.ld(Accumulator, Immediate).count().run()
            0xA5 => current.ld(Accumulator, ZeroPage(None)).count().run()
            0xB5 => current.ld(Accumulator, ZeroPage(X)).count().run()
            0xAD => current.ld(Accumulator, Absolute(None)).count().run()
            0xBD => current.ld(Accumulator, Absolute(X)).count().run()
            0xB9 => current.ld(Accumulator, Absolute(Y)).count().run()
            0xA1 => current.ld(Accumulator, IndexedIndirect(X)).count().run()
            0xB1 => current.ld(Accumulator, IndirectIndexed(Y)).count().run()
            0xA2 => current.ld(X, Immediate).count().run()
            0xAE => current.ld(X, Absolute(None)).count().run()
            0xBE => current.ld(X, Absolute(Y)).count().run()
            0xA6 => current.ld(X, ZeroPage(None)).count().run()
            0xB6 => current.ld(X, ZeroPage(Y)).count().run()
            0xA0 => current.ld(Y, Immediate).count().run()
            0xAC => current.ld(Y, Absolute(None)).count().run()
            0xBC => current.ld(Y, Absolute(X)).count().run()
            0xA4 => current.ld(Y, ZeroPage(None)).count().run()
            0xB4 => current.ld(Y, ZeroPage(X)).count().run()
            0x85 => current.st(Accumulator, ZeroPage(None)).count().run()
            0x95 => current.st(Accumulator, ZeroPage(X)).count().run()
            0x8D => current.st(Accumulator, Absolute(None)).count().run()
            0x9D => current.st(Accumulator, Absolute(X)).count().run()
            0x99 => current.st(Accumulator, Absolute(Y)).count().run()
            0x81 => current.st(Accumulator, IndexedIndirect(X)).count().run()
            0x91 => current.st(Accumulator, IndirectIndexed(Y)).count().run()
            0x86 => current.st(X, ZeroPage(None)).count().run()
            0x8E => current.st(X, Absolute(None)).count().run()
            0x96 => current.st(X, ZeroPage(Y)).count().run()
            0x84 => current.st(Y, ZeroPage(None)).count().run()
            0x8C => current.st(Y, Absolute(None)).count().run()
            0x94 => current.st(Y, ZeroPage(X)).count().run()
            0xAA => current.ta(X).run()
            0x00 => current
            _ => crash("Unimplemented opcode ${code.to_str()}")
        }
    }

    reset : Cpu -> Cpu
    reset = |cpu|
        { ..cpu,
            register: {
                program_counter: Memory.read16(cpu.memory, 0xFFFC),
                stack_pointer: 0,
                accumulator: 0,
                status: 0,
                x: 0,
                y: 0,
            },
        }

    boot : Cpu, List(U8) -> Cpu
    boot = |cpu, program| {
        loaded = { ..cpu, memory: Memory.load(cpu.memory, program) }
        loaded.reset().run()
    }

    new : {} -> Cpu
    new = |_| {
        register: {
            program_counter: 0,
            stack_pointer: 0,
            accumulator: 0,
            status: 0,
            x: 0,
            y: 0,
        },
        memory: List.repeat(0, 0xFFFF),
    }
}

expect {
    cpu = Cpu.new({}).boot([0xA9, 0x05, 0x00])
    cpu.register.accumulator == 5 and cpu.register.status.bitwise_and(0b0000_0010) == 0 and cpu.register.status.bitwise_and(0b1000_0000) == 0
}

expect {
    cpu = Cpu.new({}).boot([0xA9, 0x00, 0x00])
    cpu.register.status.bitwise_and(0b0000_0010) == 0b10
}

expect {
    cpu = Cpu.new({}).boot([0xA9, 0x0A, 0xAA, 0x00])
    cpu.register.x == 10
}

expect {
    cpu = Cpu.new({}).boot([0xA9, 0xC0, 0xAA, 0xE8, 0x00])
    cpu.register.x == 0xC1
}

expect {
    cpu = Cpu.new({}).boot([0xA9, 0xFF, 0xAA, 0xE8, 0xE8, 0x00])
    cpu.register.x == 1
}

expect {
    base = Cpu.new({})
    cpu = { ..base, memory: Memory.write8(base.memory, 0x10, 0x55) }.boot([0xA5, 0x10, 0x00])
    cpu.register.accumulator == 0x55
}
