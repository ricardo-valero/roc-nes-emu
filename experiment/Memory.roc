module [Memory, read8, write8, read16, write16]

Memory : List U8

read8 : Memory, U16 -> U8
read8 = \memory, addr ->
    List.get memory (Num.intCast addr) |> Result.withDefault 0

write8 : Memory, U16, U8 -> Memory
write8 = \memory, addr, data ->
    List.set memory (Num.intCast addr) data

read16 : Memory, U16 -> U16
read16 = \memory, pos ->
    lo = read8 memory pos
    hi = read8 memory (pos + 1)
    Num.bitwiseOr ((Num.toU16 hi) |> Num.shiftLeftBy 8) (Num.toU16 lo)

write16 : Memory, U16, U16 -> Memory
write16 = \memory, pos, data ->
    hi = Num.shiftRightZfBy data 8 |> Num.toU8
    lo = Num.bitwiseAnd data 0xFF |> Num.toU8
    memory |> write8 pos lo |> write8 (pos + 1) hi
