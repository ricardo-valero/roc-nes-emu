module [Register, Member8, read8, write8, read16, write16]

Register : {
    programCounter : U16,
    stackPointer : U8,
    accumulator : U8,
    status : U8,
    x : U8,
    y : U8,
}

Member8 : [StackPointer, Accumulator, Status, X, Y]

read8 : Register, Member8 -> U8
read8 = \reg, member ->
    when member is
        StackPointer -> reg |> .stackPointer
        Accumulator -> reg |> .accumulator
        Status -> reg |> .status
        X -> reg |> .x
        Y -> reg |> .y

write8 : Register, Member8, U8 -> Register
write8 = \reg, member, value ->
    when member is
        StackPointer -> reg |> &stackPointer value
        Accumulator -> reg |> &accumulator value
        Status -> reg |> &status value
        X -> reg |> &x value
        Y -> reg |> &y value

Member16 : [ProgramCounter]

read16 : Register, Member16 -> U16
read16 = \reg, member ->
    when member is
        ProgramCounter -> reg |> .programCounter

write16 : Register, Member16, U16 -> Register
write16 = \reg, member, value ->
    when member is
        ProgramCounter -> reg |> &programCounter value
