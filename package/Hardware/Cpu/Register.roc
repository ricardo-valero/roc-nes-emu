module []

Member8 : [StackPointer, Accumulator, X, Y, Status]
Member16 : [ProgramCounter]

Register : {
    programCounter : U16,
    stackPointer : U8,
    accumulator : U8,
    x : U8,
    y : U8,
    status : U8,
}

read8 : Member8 -> (Register -> U8)
read8 = \member -> \reg ->
        when member is
            StackPointer -> reg.stackPointer
            Accumulator -> reg.accumulator
            X -> reg.x
            Y -> reg.y
            Status -> reg.status

write8 : Member8, U8 -> (Register -> Register)
write8 = \member, byte -> \reg ->
        when member is
            Accumulator -> { reg & accumulator: byte }
            X -> { reg & x: byte }
            Y -> { reg & y: byte }
            StackPointer -> { reg & stackPointer: byte }
            Status -> { reg & status: byte }

read16 : Register, Member16 -> U16
read16 = \reg, member ->
    when member is
        ProgramCounter -> reg.programCounter
