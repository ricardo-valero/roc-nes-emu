import /Cpu/Register/Status

# 2A03 register file
# A: accumulator, X/Y: index registers, S: stack pointer,
# P: status (flags), PC: program counter
Register := {
    program_counter : U16,
    stack_pointer : U8,
    accumulator : U8,
    x : U8,
    y : U8,
    status : U8,
}.{
    Member16 := [ProgramCounter]
    Member8 := [StackPointer, Accumulator, X, Y, Status]

    # 2A03 power-up state (A/X/Y zero, S = 0xFD, P = 0x24: interrupt-disable + unused bit)
    init : {} -> Register
    init = |_| {
        program_counter: 0x0000,
        stack_pointer: 0xFD,
        accumulator: 0x00,
        x: 0x00,
        y: 0x00,
        status: 0x24,
    }

    read16 : Register, Member16 -> U16
    read16 = |reg, member|
        match member {
            ProgramCounter => reg.program_counter
        }

    write16 : Register, Member16, U16 -> Register
    write16 = |reg, member, value|
        match member {
            ProgramCounter => { ..reg, program_counter: value }
        }

    read8 : Register, Member8 -> U8
    read8 = |reg, member|
        match member {
            StackPointer => reg.stack_pointer
            Accumulator => reg.accumulator
            X => reg.x
            Y => reg.y
            Status => reg.status
        }

    write8 : Register, Member8, U8 -> Register
    write8 = |reg, member, value|
        match member {
            StackPointer => { ..reg, stack_pointer: value }
            Accumulator => { ..reg, accumulator: value }
            X => { ..reg, x: value }
            Y => { ..reg, y: value }
            Status => { ..reg, status: value }
        }

    # Structural copies of Status.Member below: the nightly compiler cannot yet
    # reference nested types through subdirectory imports. Values still flow
    # into the nominal type at call sites.
    read_status : Register, [Carry, Zero, InterruptDisable, DecimalMode, Break, Overflow, Negative] -> Bool
    read_status = |reg, member| {
        checked = Status.check(member)
        checked(reg.status)
    }

    write_status : Register, [Carry, Zero, InterruptDisable, DecimalMode, Break, Overflow, Negative], Bool -> Register
    write_status = |reg, member, value| {
        modified = Status.modify(member, value)
        { ..reg, status: modified(reg.status) }
    }
}

expect Register.init({}).read8(StackPointer) == 0xFD
expect Register.init({}).write8(Accumulator, 0x42).read8(Accumulator) == 0x42
expect Register.init({}).write16(ProgramCounter, 0x8000).read16(ProgramCounter) == 0x8000
expect Register.init({}).write_status(Carry, Bool.True).read_status(Carry) == Bool.True
