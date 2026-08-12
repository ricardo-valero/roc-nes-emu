import /Cpu/Register/Status

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

    read16 : Member16 -> (Register -> U16)
    read16 = |member| |reg|
        match member {
            ProgramCounter => reg.program_counter
        }

    write16 : Member16, U16 -> (Register -> Register)
    write16 = |member, value| |reg|
        match member {
            ProgramCounter => { ..reg, program_counter: value }
        }

    read8 : Member8 -> (Register -> U8)
    read8 = |member| |reg|
        match member {
            StackPointer => reg.stack_pointer
            Accumulator => reg.accumulator
            X => reg.x
            Y => reg.y
            Status => reg.status
        }

    write8 : Member8, U8 -> (Register -> Register)
    write8 = |member, value| |reg|
        match member {
            Accumulator => { ..reg, accumulator: value }
            X => { ..reg, x: value }
            Y => { ..reg, y: value }
            StackPointer => { ..reg, stack_pointer: value }
            Status => { ..reg, status: value }
        }

    # Structural copies of Status.Member below: the nightly compiler cannot yet
    # reference nested types through subdirectory imports. Values still flow
    # into the nominal type at call sites.
    read_status : [Carry, Zero, InterruptDisable, DecimalMode, Break, Overflow, Negative] -> (Register -> Bool)
    read_status = |member| |reg| {
        get = read8(Status)
        checked = Status.check(member)
        checked(get(reg))
    }

    write_status : [Carry, Zero, InterruptDisable, DecimalMode, Break, Overflow, Negative], Bool -> (Register -> Register)
    write_status = |member, value| |reg| {
        modified = Status.modify(member, value)
        put = write8(Status, modified(reg.status))
        put(reg)
    }
}
