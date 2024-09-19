module [Cpu, boot]

import Memory exposing [Memory]
import Register exposing [Register]

Cpu : {
    register : Register,
    memory : Memory,
}

AddressingMode : [
    Immediate,
    Absolute [None, X, Y],
    ZeroPage [None, X, Y],
    IndexedIndirect [X],
    IndirectIndexed [Y],
]

getOperandAddress : Cpu, AddressingMode -> U16
getOperandAddress = \cpu, mode ->
    when mode is
        Immediate -> cpu.register.programCounter
        Absolute m ->
            offset =
                when m is
                    X -> cpu.register.x
                    Y -> cpu.register.y
                    _ -> 0
            base = Memory.read16 cpu.memory cpu.register.programCounter
            base |> Num.addWrap (offset |> Num.toU16)

        ZeroPage m ->
            offset =
                when m is
                    X -> cpu.register.x
                    Y -> cpu.register.y
                    _ -> 0
            pos = Memory.read8 cpu.memory cpu.register.programCounter
            pos |> Num.addWrap offset |> Num.toU16

        IndexedIndirect m ->
            offset =
                when m is
                    X -> cpu.register.x
            base = Memory.read8 cpu.memory cpu.register.programCounter
            ptr = base |> Num.addWrap offset
            lo = Memory.read8 cpu.memory (ptr |> Num.toU16)
            hi = Memory.read8 cpu.memory (ptr |> Num.addWrap 1 |> Num.toU16)
            Num.bitwiseOr (hi |> Num.toU16 |> Num.shiftLeftBy 8) (lo |> Num.toU16)

        IndirectIndexed m ->
            offset =
                when m is
                    Y -> cpu.register.y
            base = Memory.read8 cpu.memory cpu.register.programCounter
            lo = Memory.read8 cpu.memory (base |> Num.toU16)
            hi = Memory.read8 cpu.memory (base |> Num.addWrap 1 |> Num.toU16)
            derefBase = Num.bitwiseOr (hi |> Num.toU16 |> Num.shiftLeftBy 8) (lo |> Num.toU16)
            deref = derefBase |> Num.addWrap (offset |> Num.toU16)
            deref

or : Cpu, AddressingMode -> Cpu
or = \cpu, mode ->
    addr = getOperandAddress cpu mode
    value = Memory.read8 cpu.memory addr
    result = value |> Num.bitwiseOr cpu.register.accumulator
    cpu |> &register (cpu.register |> &accumulator result)

and : Cpu, AddressingMode -> Cpu
and = \cpu, mode ->
    addr = getOperandAddress cpu mode
    value = Memory.read8 cpu.memory addr
    result = value |> Num.bitwiseAnd cpu.register.accumulator
    cpu |> &register (cpu.register |> &accumulator result)

xor : Cpu, AddressingMode -> Cpu
xor = \cpu, mode ->
    addr = getOperandAddress cpu mode
    value = Memory.read8 cpu.memory addr
    result = value |> Num.bitwiseXor cpu.register.accumulator
    cpu |> &register (cpu.register |> &accumulator result)

# lda, ldx, ldy
ld : Cpu, [A, X, Y], AddressingMode -> Cpu
ld = \cpu, m, mode ->
    addr = cpu |> getOperandAddress mode
    data = cpu.memory |> Memory.read8 addr
    reg =
        when m is
            A -> cpu.register |> &accumulator data
            X -> cpu.register |> &x data
            Y -> cpu.register |> &y data
    cpu |> &register (reg |> updateFlags data)

sta : Cpu, AddressingMode -> Cpu
sta = \cpu, mode ->
    addr = getOperandAddress cpu mode
    mem = cpu.memory |> Memory.write8 addr cpu.register.accumulator
    cpu |> &memory mem

# tax, tay
ta : Cpu, [X, Y] -> Cpu
ta = \cpu, m ->
    when m is
        X ->
            reg = cpu.register |> &x cpu.register.accumulator
            cpu |> &register (updateFlags reg reg.x)

        Y ->
            reg = cpu.register |> &y cpu.register.accumulator
            cpu |> &register (updateFlags reg reg.y)

inc = \cpu, mode ->
    addr = getOperandAddress cpu mode
    data = Memory.read8 cpu.memory addr
    result = data |> Num.addWrap 1
    cpu
    |> &memory (cpu.memory |> Memory.write8 addr result)
    |> &register (cpu.register |> updateFlags result)

dec = \cpu, mode ->
    addr = getOperandAddress cpu mode
    data = Memory.read8 cpu.memory addr
    result = data |> Num.subWrap 1
    cpu
    |> &memory (cpu.memory |> Memory.write8 addr result)
    |> &register (cpu.register |> updateFlags result)

# inx, iny
in : Cpu, [X, Y] -> Cpu
in = \cpu, m ->
    reg =
        when m is
            X ->
                result = cpu.register |> .x |> Num.addWrap 1
                cpu.register |> &x result |> updateFlags result

            Y ->
                result = cpu.register |> .y |> Num.addWrap 1
                cpu.register |> &y result |> updateFlags result
    cpu |> &register reg

# dex, dey
de : Cpu, [X, Y] -> Cpu
de = \cpu, m ->
    reg =
        when m is
            X ->
                result = cpu.register.x |> Num.subWrap 1
                cpu.register |> &x result |> updateFlags result

            Y ->
                result = cpu.register.y |> Num.subWrap 1
                cpu.register |> &y result |> updateFlags result
    cpu |> &register reg

updateFlags : Register, U8 -> Register
updateFlags = \reg, result ->
    newStatus = if result == 0 then Num.bitwiseOr reg.status 0b0000_0010 else Num.bitwiseAnd reg.status 0b1111_1101
    newStatus2 = if Num.bitwiseAnd result 0b1000_0000 != 0 then Num.bitwiseOr newStatus 0b1000_0000 else Num.bitwiseAnd newStatus 0b0111_1111
    { reg & status: newStatus2 }

count = \c -> c |> &register (c.register |> &programCounter (Num.addWrap c.register.programCounter 1))

run : Cpu -> Cpu
run = \cpu ->
    code = Memory.read8 cpu.memory cpu.register.programCounter
    cpu
    |> count
    |> \current ->
        when code is
            0x09 -> current |> or Immediate |> run
            0x0D -> current |> or (Absolute None) |> run
            0x1D -> current |> or (Absolute X) |> run
            0x19 -> current |> or (Absolute Y) |> run
            0x05 -> current |> or (ZeroPage None) |> run
            0x15 -> current |> or (ZeroPage X) |> run
            0x01 -> current |> or (IndexedIndirect X) |> run
            0x11 -> current |> or (IndirectIndexed Y) |> run
            0x29 -> current |> and Immediate |> run
            0x2D -> current |> and (Absolute None) |> run
            0x3D -> current |> and (Absolute X) |> run
            0x39 -> current |> and (Absolute Y) |> run
            0x25 -> current |> and (ZeroPage None) |> run
            0x35 -> current |> and (ZeroPage X) |> run
            0x21 -> current |> and (IndexedIndirect X) |> run
            0x31 -> current |> and (IndirectIndexed Y) |> run
            0x49 -> current |> xor Immediate |> run
            0x4D -> current |> xor (Absolute None) |> run
            0x5D -> current |> xor (Absolute X) |> run
            0x59 -> current |> xor (Absolute Y) |> run
            0x45 -> current |> xor (ZeroPage None) |> run
            0x55 -> current |> xor (ZeroPage X) |> run
            0x41 -> current |> xor (IndexedIndirect X) |> run
            0x51 -> current |> xor (IndirectIndexed Y) |> run
            0xE8 -> current |> in X |> run
            0xC8 -> current |> in Y |> run
            0xE6 -> current |> inc (ZeroPage None) |> run
            0xEE -> current |> inc (Absolute None) |> run
            0xF6 -> current |> inc (ZeroPage X) |> run
            0xFE -> current |> inc (Absolute X) |> run
            0xCA -> current |> de X |> run
            0x88 -> current |> de Y |> run
            0xC6 -> current |> dec (ZeroPage None) |> run
            0xCE -> current |> dec (Absolute None) |> run
            0xD6 -> current |> dec (ZeroPage X) |> run
            0xDE -> current |> dec (Absolute X) |> run
            0xA9 -> current |> ld A Immediate |> count |> run
            0xA5 -> current |> ld A (ZeroPage None) |> count |> run
            0xB5 -> current |> ld A (ZeroPage X) |> count |> run
            0xAD -> current |> ld A (Absolute None) |> count |> run
            0xBD -> current |> ld A (Absolute X) |> count |> run
            0xB9 -> current |> ld A (Absolute Y) |> count |> run
            0xA1 -> current |> ld A (IndexedIndirect X) |> count |> run
            0xB1 -> current |> ld A (IndirectIndexed Y) |> count |> run
            0xA2 -> current |> ld X Immediate |> count |> run
            0xAE -> current |> ld X (Absolute None) |> count |> run
            0xBE -> current |> ld X (Absolute Y) |> count |> run
            0xA6 -> current |> ld X (ZeroPage None) |> count |> run
            0xB6 -> current |> ld X (ZeroPage Y) |> count |> run
            0xA0 -> current |> ld Y Immediate |> count |> run
            0xAC -> current |> ld Y (Absolute None) |> count |> run
            0xBC -> current |> ld Y (Absolute X) |> count |> run
            0xA4 -> current |> ld Y (ZeroPage None) |> count |> run
            0xB4 -> current |> ld Y (ZeroPage X) |> count |> run
            0x85 -> current |> sta (ZeroPage None) |> count |> run
            0x95 -> current |> sta (ZeroPage X) |> count |> run
            0x8D -> current |> sta (Absolute None) |> count |> run
            0x9D -> current |> sta (Absolute X) |> count |> run
            0x99 -> current |> sta (Absolute Y) |> count |> run
            0x81 -> current |> sta (IndexedIndirect X) |> count |> run
            0x91 -> current |> sta (IndirectIndexed Y) |> count |> run
            0xAA -> current |> ta X |> run
            0x00 -> current
            _ -> crash "Unimplemented opcode $(Num.toStr code)"

load : Cpu, List U8 -> Cpu
load = \cpu, program ->
    helper = \currentMem, index ->
        if index >= List.len program then
            currentMem
        else
            updatedMem = Memory.write8 currentMem (0x8000 + Num.toU16 index) (List.get program index |> Result.withDefault 0)
            helper updatedMem (index + 1)

    mem = Memory.write16 (helper cpu.memory 0) 0xFFFC 0x8000
    cpu |> &memory mem

reset : Cpu -> Cpu
reset = \cpu ->
    { cpu &
        register: {
            programCounter: Memory.read16 cpu.memory 0xFFFC,
            accumulator: 0,
            status: 0,
            x: 0,
            y: 0,
        },
    }

boot : Cpu, List U8 -> Cpu
boot = \cpu, program ->
    cpu
    |> load program
    |> reset
    |> run

new : Cpu
new = {
    register: {
        programCounter: 0,
        accumulator: 0,
        status: 0,
        x: 0,
        y: 0,
    },
    memory: List.repeat 0 0xFFFF,
}

expect
    cpu = new |> boot [0xA9, 0x05, 0x00]
    cpu.register.accumulator == 5 && (Num.bitwiseAnd cpu.register.status 0b0000_0010 == 0) && (Num.bitwiseAnd cpu.register.status 0b1000_0000 == 0)

expect
    cpu = new |> boot [0xA9, 0x00, 0x00]
    Num.bitwiseAnd cpu.register.status 0b0000_0010 == 0b10

expect
    cpu = new |> boot [0xA9, 0x0A, 0xAA, 0x00]
    cpu.register.x == 10

expect
    cpu = new |> boot [0xA9, 0xC0, 0xAA, 0xE8, 0x00]
    cpu.register.x == 0xC1

expect
    cpu = new |> boot [0xA9, 0xFF, 0xAA, 0xE8, 0xE8, 0x00]
    cpu.register.x == 1

expect
    cpu = new |> &memory (Memory.write8 new.memory 0x10 0x55) |> boot [0xA5, 0x10, 0x00]
    cpu.register.accumulator == 0x55
