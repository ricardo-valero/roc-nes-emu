Register :: [].{
    Type : {
        program_counter : U16,
        stack_pointer : U8,
        accumulator : U8,
        status : U8,
        x : U8,
        y : U8,
    }

    Member8 := [StackPointer, Accumulator, Status, X, Y]

    read8 : Type, Member8 -> U8
    read8 = |reg, member|
        match member {
            StackPointer => reg.stack_pointer
            Accumulator => reg.accumulator
            Status => reg.status
            X => reg.x
            Y => reg.y
        }

    write8 : Type, Member8, U8 -> Type
    write8 = |reg, member, value|
        match member {
            StackPointer => { ..reg, stack_pointer: value }
            Accumulator => { ..reg, accumulator: value }
            Status => { ..reg, status: value }
            X => { ..reg, x: value }
            Y => { ..reg, y: value }
        }

    Member16 := [ProgramCounter]

    read16 : Type, Member16 -> U16
    read16 = |reg, member|
        match member {
            ProgramCounter => reg.program_counter
        }

    write16 : Type, Member16, U16 -> Type
    write16 = |reg, member, value|
        match member {
            ProgramCounter => { ..reg, program_counter: value }
        }
}
