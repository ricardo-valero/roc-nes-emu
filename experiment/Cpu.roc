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
    programCounter = cpu.register |> Register.read16 ProgramCounter
    when mode is
        Immediate -> programCounter
        Absolute m ->
            offset =
                when m is
                    X -> cpu.register |> Register.read8 X
                    Y -> cpu.register |> Register.read8 Y
                    _ -> 0
            base = Memory.read16 cpu.memory programCounter
            base |> Num.addWrap (offset |> Num.toU16)

        ZeroPage m ->
            offset =
                when m is
                    X -> cpu.register |> Register.read8 X
                    Y -> cpu.register |> Register.read8 Y
                    _ -> 0
            pos = Memory.read8 cpu.memory programCounter
            pos |> Num.addWrap offset |> Num.toU16

        IndexedIndirect m ->
            offset =
                when m is
                    X -> cpu.register |> Register.read8 X
            base = Memory.read8 cpu.memory programCounter
            ptr = base |> Num.addWrap offset
            lo = Memory.read8 cpu.memory (ptr |> Num.toU16)
            hi = Memory.read8 cpu.memory (ptr |> Num.addWrap 1 |> Num.toU16)
            Num.bitwiseOr (hi |> Num.toU16 |> Num.shiftLeftBy 8) (lo |> Num.toU16)

        IndirectIndexed m ->
            offset =
                when m is
                    Y -> cpu.register |> Register.read8 Y
            base = Memory.read8 cpu.memory programCounter
            lo = Memory.read8 cpu.memory (base |> Num.toU16)
            hi = Memory.read8 cpu.memory (base |> Num.addWrap 1 |> Num.toU16)
            derefBase = Num.bitwiseOr (hi |> Num.toU16 |> Num.shiftLeftBy 8) (lo |> Num.toU16)
            deref = derefBase |> Num.addWrap (offset |> Num.toU16)
            deref

or : Cpu, AddressingMode -> Cpu
or = \cpu, mode ->
    addr = getOperandAddress cpu mode
    value = Memory.read8 cpu.memory addr
    result = value |> Num.bitwiseOr (cpu.register |> Register.read8 Accumulator)
    cpu |> &register (cpu.register |> Register.write8 Accumulator result)

and : Cpu, AddressingMode -> Cpu
and = \cpu, mode ->
    addr = getOperandAddress cpu mode
    value = Memory.read8 cpu.memory addr
    result = value |> Num.bitwiseAnd (cpu.register |> Register.read8 Accumulator)
    cpu |> &register (cpu.register |> Register.write8 Accumulator result)

xor : Cpu, AddressingMode -> Cpu
xor = \cpu, mode ->
    addr = getOperandAddress cpu mode
    value = Memory.read8 cpu.memory addr
    result = value |> Num.bitwiseXor (cpu.register |> Register.read8 Accumulator)
    cpu |> &register (cpu.register |> Register.write8 Accumulator result)

# sta, stx, sty
st : Cpu, Register.Member8, AddressingMode -> Cpu
st = \cpu, m, mode ->
    addr = getOperandAddress cpu mode
    mem = cpu.memory |> Memory.write8 addr (cpu.register |> Register.read8 m)
    cpu |> &memory mem

# lda, ldx, ldy
ld : Cpu, Register.Member8, AddressingMode -> Cpu
ld = \cpu, m, mode ->
    addr = cpu |> getOperandAddress mode
    data = cpu.memory |> Memory.read8 addr
    reg = cpu.register |> Register.write8 m data
    cpu |> &register (reg |> updateFlags data)

# tax, tay
ta : Cpu, Register.Member8 -> Cpu
ta = \cpu, m ->
    data = cpu.register.accumulator
    reg = cpu.register |> Register.write8 m data
    cpu |> &register (reg |> updateFlags data)

inc : Cpu, AddressingMode -> Cpu
inc = \cpu, mode ->
    addr = getOperandAddress cpu mode
    data = Memory.read8 cpu.memory addr
    result = data |> Num.addWrap 1
    cpu
    |> &memory (cpu.memory |> Memory.write8 addr result)
    |> &register (cpu.register |> updateFlags result)

dec : Cpu, AddressingMode -> Cpu
dec = \cpu, mode ->
    addr = getOperandAddress cpu mode
    data = Memory.read8 cpu.memory addr
    result = data |> Num.subWrap 1
    cpu
    |> &memory (cpu.memory |> Memory.write8 addr result)
    |> &register (cpu.register |> updateFlags result)

# inx, iny
in : Cpu, Register.Member8 -> Cpu
in = \cpu, m ->
    data = cpu.register |> Register.read8 m
    result = data |> Num.addWrap 1
    reg = cpu.register |> Register.write8 m result
    cpu |> &register (reg |> updateFlags result)

# dex, dey
de : Cpu, Register.Member8 -> Cpu
de = \cpu, m ->
    data = cpu.register |> Register.read8 m
    result = data |> Num.subWrap 1
    reg = cpu.register |> Register.write8 m result
    cpu |> &register (reg |> updateFlags result)

updateFlags : Register, U8 -> Register
updateFlags = \reg, result ->
    newStatus = if result == 0 then Num.bitwiseOr reg.status 0b0000_0010 else Num.bitwiseAnd reg.status 0b1111_1101
    newStatus2 = if Num.bitwiseAnd result 0b1000_0000 != 0 then Num.bitwiseOr newStatus 0b1000_0000 else Num.bitwiseAnd newStatus 0b0111_1111
    reg |> Register.write8 Status newStatus2

count = \c -> c |> &register (c.register |> Register.write16 ProgramCounter (c.register |> Register.read16 ProgramCounter |> Num.addWrap 1))

run : Cpu -> Cpu
run = \cpu ->
    code = Memory.read8 cpu.memory (cpu.register |> Register.read16 ProgramCounter)
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
            0xA9 -> current |> ld Accumulator Immediate |> count |> run
            0xA5 -> current |> ld Accumulator (ZeroPage None) |> count |> run
            0xB5 -> current |> ld Accumulator (ZeroPage X) |> count |> run
            0xAD -> current |> ld Accumulator (Absolute None) |> count |> run
            0xBD -> current |> ld Accumulator (Absolute X) |> count |> run
            0xB9 -> current |> ld Accumulator (Absolute Y) |> count |> run
            0xA1 -> current |> ld Accumulator (IndexedIndirect X) |> count |> run
            0xB1 -> current |> ld Accumulator (IndirectIndexed Y) |> count |> run
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
            0x85 -> current |> st Accumulator (ZeroPage None) |> count |> run
            0x95 -> current |> st Accumulator (ZeroPage X) |> count |> run
            0x8D -> current |> st Accumulator (Absolute None) |> count |> run
            0x9D -> current |> st Accumulator (Absolute X) |> count |> run
            0x99 -> current |> st Accumulator (Absolute Y) |> count |> run
            0x81 -> current |> st Accumulator (IndexedIndirect X) |> count |> run
            0x91 -> current |> st Accumulator (IndirectIndexed Y) |> count |> run
            0x86 -> current |> st X (ZeroPage None) |> count |> run
            0x8E -> current |> st X (Absolute None) |> count |> run
            0x96 -> current |> st X (ZeroPage Y) |> count |> run
            0x84 -> current |> st Y (ZeroPage None) |> count |> run
            0x8C -> current |> st Y (Absolute None) |> count |> run
            0x94 -> current |> st Y (ZeroPage X) |> count |> run
            0xAA -> current |> ta X |> run
            0x00 -> current
            _ -> crash "Unimplemented opcode $(Num.toStr code)"

reset : Cpu -> Cpu
reset = \cpu ->
    cpu
    |> &register {
        programCounter: Memory.read16 cpu.memory 0xFFFC,
        stackPointer: 0,
        accumulator: 0,
        status: 0,
        x: 0,
        y: 0,
    }

boot : Cpu, List U8 -> Cpu
boot = \cpu, program ->
    cpu
    |> &memory (cpu.memory |> Memory.load program)
    |> reset
    |> run

new : Cpu
new = {
    register: {
        programCounter: 0,
        stackPointer: 0,
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
