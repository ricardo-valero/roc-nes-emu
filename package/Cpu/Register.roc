module [read16, write16, read8, write8, readStatus, writeStatus]

Member16 : [ProgramCounter]
Member8 : [StackPointer, Accumulator, X, Y, Status]

import Cpu.Register.Status as Status

Register : {
    programCounter : U16,
    stackPointer : U8,
    accumulator : U8,
    x : U8,
    y : U8,
    status : U8,
}

read16 : Member16 -> (Register -> U16)
read16 = \member -> \reg ->
        when member is
            ProgramCounter -> reg.programCounter

write16 : Member16, U16 -> (Register -> Register)
write16 = \member, value -> \reg ->
        when member is
            ProgramCounter -> { reg & programCounter: value }

read8 : Member8 -> (Register -> U8)
read8 = \member -> \reg ->
        when member is
            StackPointer -> reg.stackPointer
            Accumulator -> reg.accumulator
            X -> reg.x
            Y -> reg.y
            Status -> reg.status

write8 : Member8, U8 -> (Register -> Register)
write8 = \member, value -> \reg ->
        when member is
            Accumulator -> { reg & accumulator: value }
            X -> { reg & x: value }
            Y -> { reg & y: value }
            StackPointer -> { reg & stackPointer: value }
            Status -> { reg & status: value }

readStatus : Status.Member -> (Register -> Bool)
readStatus = \member -> \reg ->
        reg |> (read8 Status) |> (Status.check member)

writeStatus : Status.Member, Bool -> (Register -> Register)
writeStatus = \member, value -> \reg ->
        reg |> (write8 Status (reg.status |> (Status.modify member value)))
