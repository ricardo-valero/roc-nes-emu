module [Cpu]

import Memory exposing [Memory]
import Register exposing [Register]

Cpu : {
    register : Register,
    memory : Memory,
}

newCpu : Cpu
newCpu = {
    register: {
        programCounter: 0,
        accumulator: 0,
        status: 0,
        x: 0,
        y: 0,
    },
    memory: List.repeat 0 0xFFFF,
}

AddressingMode : [Immediate, ZeroPage, ZeroPageX, ZeroPageY, Absolute, AbsoluteX, AbsoluteY, XIndirect, IndirectY]

getOperandAddress : Cpu, AddressingMode -> U16
getOperandAddress = \cpu, mode ->
    when mode is
        Immediate -> cpu.register.programCounter
        ZeroPage -> Memory.read8 cpu.memory cpu.register.programCounter |> Num.toU16
        Absolute -> Memory.read16 cpu.memory cpu.register.programCounter
        ZeroPageX ->
            pos = Memory.read8 cpu.memory cpu.register.programCounter
            pos |> Num.addWrap cpu.register.x |> Num.toU16

        ZeroPageY ->
            pos = Memory.read8 cpu.memory cpu.register.programCounter
            pos |> Num.addWrap cpu.register.y |> Num.toU16

        AbsoluteX ->
            base = Memory.read16 cpu.memory cpu.register.programCounter
            base |> Num.addWrap (cpu.register.x |> Num.toU16)

        AbsoluteY ->
            base = Memory.read16 cpu.memory cpu.register.programCounter
            base |> Num.addWrap (cpu.register.y |> Num.toU16)

        XIndirect ->
            base = Memory.read8 cpu.memory cpu.register.programCounter
            ptr = base |> Num.addWrap cpu.register.x
            lo = Memory.read8 cpu.memory (ptr |> Num.toU16)
            hi = Memory.read8 cpu.memory (ptr |> Num.addWrap 1 |> Num.toU16)
            Num.bitwiseOr (hi |> Num.toU16 |> Num.shiftLeftBy 8) (lo |> Num.toU16)

        IndirectY ->
            base = Memory.read8 cpu.memory cpu.register.programCounter
            lo = Memory.read8 cpu.memory (base |> Num.toU16)
            hi = Memory.read8 cpu.memory (base |> Num.addWrap 1 |> Num.toU16)
            derefBase = Num.bitwiseOr (hi |> Num.toU16 |> Num.shiftLeftBy 8) (lo |> Num.toU16)
            deref = derefBase |> Num.addWrap (cpu.register.y |> Num.toU16)
            deref

lda : Cpu, AddressingMode -> Cpu
lda = \cpu, mode ->
    addr = getOperandAddress cpu mode
    value = Memory.read8 cpu.memory addr
    reg = cpu.register |> &accumulator value
    cpu |> &register (updateZeroAndNegativeFlags reg reg.accumulator)

sta : Cpu, AddressingMode -> Cpu
sta = \cpu, mode ->
    addr = getOperandAddress cpu mode
    mem = Memory.write8 cpu.memory addr cpu.register.accumulator
    cpu |> &memory mem

tax : Cpu -> Cpu
tax = \cpu ->
    reg = cpu.register |> &x cpu.register.accumulator
    cpu |> &register (updateZeroAndNegativeFlags reg reg.x)

inx : Cpu -> Cpu
inx = \cpu ->
    reg = cpu.register |> &x (Num.addWrap cpu.register.x 1)
    cpu |> &register (updateZeroAndNegativeFlags reg reg.x)

updateZeroAndNegativeFlags : Register, U8 -> Register
updateZeroAndNegativeFlags = \reg, result ->
    newStatus =
        if result == 0 then
            Num.bitwiseOr reg.status 0b0000_0010
        else
            Num.bitwiseAnd reg.status 0b1111_1101

    newStatus2 =
        if Num.bitwiseAnd result 0b1000_0000 != 0 then
            Num.bitwiseOr newStatus 0b1000_0000
        else
            Num.bitwiseAnd newStatus 0b0111_1111

    { reg & status: newStatus2 }

run : Cpu -> Cpu
run = \cpu ->
    count = \c -> c |> &register (c.register |> &programCounter (Num.addWrap c.register.programCounter 1))
    helper = \currentCpu ->
        code = Memory.read8 currentCpu.memory currentCpu.register.programCounter
        when code is
            0xA9 -> currentCpu |> count |> lda Immediate |> count |> helper
            0xA5 -> currentCpu |> count |> lda ZeroPage |> count |> helper
            0xB5 -> currentCpu |> count |> lda ZeroPageX |> count |> helper
            0xAD -> currentCpu |> count |> lda Absolute |> count |> helper
            0xBD -> currentCpu |> count |> lda AbsoluteX |> count |> helper
            0xB9 -> currentCpu |> count |> lda AbsoluteY |> count |> helper
            0xA1 -> currentCpu |> count |> lda XIndirect |> count |> helper
            0xB1 -> currentCpu |> count |> lda IndirectY |> count |> helper
            0x85 -> currentCpu |> count |> sta ZeroPage |> count |> helper
            0x95 -> currentCpu |> count |> sta ZeroPageX |> count |> helper
            0x8D -> currentCpu |> count |> sta Absolute |> count |> helper
            0x9D -> currentCpu |> count |> sta AbsoluteX |> count |> helper
            0x99 -> currentCpu |> count |> sta AbsoluteY |> count |> helper
            0x81 -> currentCpu |> count |> sta XIndirect |> count |> helper
            0x91 -> currentCpu |> count |> sta IndirectY |> count |> helper
            0xAA -> currentCpu |> count |> tax |> helper
            0xE8 -> currentCpu |> count |> inx |> helper
            0x00 -> currentCpu |> count
            _ -> crash "Unimplemented opcode $(Num.toStr code)"

    helper cpu

load : Cpu, List U8 -> Cpu
load = \cpu, program ->
    # TODO: How to make this better?
    l1 = List.split cpu.memory 0x8000
    l2 = List.split l1.others (List.len program + 1)
    mem1 = l1.before |> List.concat program |> List.concat l2.others
    updatedCpu1 = { cpu & memory: mem1 }
    mem2 = Memory.write16 updatedCpu1.memory 0xFFFC 0x8000
    { updatedCpu1 & memory: mem2 }

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

loadAndRun : Cpu, List U8 -> Cpu
loadAndRun = \cpu, program ->
    cpu
    |> load program
    |> reset
    |> run

# Tests

expect
    cpu = newCpu |> loadAndRun [0xA9, 0x05, 0x00]
    cpu.register.accumulator == 5 && (Num.bitwiseAnd cpu.register.status 0b0000_0010 == 0) && (Num.bitwiseAnd cpu.register.status 0b1000_0000 == 0)

expect
    cpu = newCpu |> loadAndRun [0xA9, 0x00, 0x00]
    Num.bitwiseAnd cpu.register.status 0b0000_0010 == 0b10

expect
    cpu = newCpu |> loadAndRun [0xA9, 0x0A, 0xAA, 0x00]
    cpu.register.x == 10

expect
    cpu = newCpu |> loadAndRun [0xA9, 0xC0, 0xAA, 0xE8, 0x00]
    cpu.register.x == 0xC1

expect
    cpu = newCpu |> loadAndRun [0xA9, 0xFF, 0xAA, 0xE8, 0xE8, 0x00]
    cpu.register.x == 1

expect
    cpu = newCpu |> &memory (Memory.write8 newCpu.memory 0x10 0x55) |> loadAndRun [0xA5, 0x10, 0x00]
    cpu.register.accumulator == 0x55

# expect
#    cpu = newCpu |> memWrite 0x10 0x55 |> loadAndRun [0xA5, 0x10, 0x00]
#    cpu.registerA == 0x55
