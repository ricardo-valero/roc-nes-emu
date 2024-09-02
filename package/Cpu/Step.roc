module [step]
# BYVAL contents of a memory location to the __
# BYREF contents of the __ into memory.
#
# Logical and arithmetic
# Adc, # Add accumulator with carry
# Sbc, # Sub accumulator with borrow (carry - 1)
#
# Renaming stuff
# Pull -> Pop
# JumpToSubroutine (JSR) -> Call

import Cpu.Register
import Cpu.Register.Status
import Memory

Step : [
    Math
        (
            [Or, And, Xor, Adc, Sbc, Compare [A, X, Y], Inc, Dec, Asl, Lsr, RotateLeft, RotateRight, Bit],
            [ByVal [Absolute, AbsoluteX, AbsoluteY, IndirectY, XIndirect, ZeroPage, ZeroPageX], Immediate, Implied [A, X, Y], Inplace [Absolute, AbsoluteX, ZeroPage, ZeroPageX]],
        ),
    Move
        (
            [Load, Store, Transfer],
            [A, SP, X, Y],
            [ByRef [Absolute, AbsoluteX, AbsoluteY, IndirectY, XIndirect, ZeroPage, ZeroPageX, ZeroPageY], ByVal [Absolute, AbsoluteX, AbsoluteY, IndirectY, XIndirect, ZeroPage, ZeroPageX, ZeroPageY], Immediate, Implied [A, SP, X, Y]],
        ),
    Stack
        (
            [Pop, Push],
            [Implied [A, Status]],
        ),
    Flow
        (
            [Jump, Call, Branch],
            [Always, On (Cpu.Register.Status.Member, Bool)],
            [ByVal [Absolute, ZeroPage], Indirect, Relative],
        ),
    Modify
        (
            [Status],
            Cpu.Register.Status.Member,
            Bool,
        ),
    Misc
        (
            [ForceInterrupt, ReturnFromInterrupt, ReturnFromSubroutine],
            [Implied [None]],
        ),
    Unknown,
]

Emulator : {
    reg : Cpu.Register.Register,
    ram : Memory.Memory,
}

fetch = \{ reg, ram } ->
    addr = reg.programCounter
    byte = Memory.read ram addr
    T { reg: { reg & programCounter: Num.addWrap addr 1 }, ram } byte

# branch : Emulator, Cpu.Register.Status.Member, Bool -> Emulator
branch = \emu, offset ->
    programCounter =
        addResult = Num.addWrap emu.reg.programCounter (Num.toU16 offset)
        if offset < 0x80 then
            addResult
        else
            Num.subWrap addResult 0x100
    { emu & reg: (Cpu.Register.write16 ProgramCounter programCounter) emu.reg }

handle : Step, Emulator -> Emulator
handle = \s, emu0 ->
    when s is
        Modify (type, member, value) ->
            when type is
                Status -> { emu0 & reg: (Cpu.Register.writeStatus member value) emu0.reg }

        Flow (type, condition, addressing) ->
            (T emu byte) = fetch emu0
            c =
                when condition is
                    Always -> Bool.true
                    On (f, b) -> (Cpu.Register.readStatus f) emu.reg == b
            if c then
                when type is
                    Branch -> branch emu byte
                    _ -> emu
            else
                emu

        _ -> emu0

step : U8 -> Step
step = \byte ->
    when byte is
        0x09 -> Math (Or, Immediate)
        0x05 -> Math (Or, ByVal ZeroPage)
        0x15 -> Math (Or, ByVal ZeroPageX)
        0x0D -> Math (Or, ByVal Absolute)
        0x1D -> Math (Or, ByVal AbsoluteX)
        0x19 -> Math (Or, ByVal AbsoluteY)
        0x01 -> Math (Or, ByVal XIndirect)
        0x11 -> Math (Or, ByVal IndirectY)
        0x29 -> Math (And, Immediate)
        0x25 -> Math (And, ByVal ZeroPage)
        0x35 -> Math (And, ByVal ZeroPageX)
        0x2D -> Math (And, ByVal Absolute)
        0x3D -> Math (And, ByVal AbsoluteX)
        0x39 -> Math (And, ByVal AbsoluteY)
        0x21 -> Math (And, ByVal XIndirect)
        0x31 -> Math (And, ByVal IndirectY)
        0x49 -> Math (Xor, Immediate)
        0x45 -> Math (Xor, ByVal ZeroPage)
        0x55 -> Math (Xor, ByVal ZeroPageX)
        0x4D -> Math (Xor, ByVal Absolute)
        0x5D -> Math (Xor, ByVal AbsoluteX)
        0x59 -> Math (Xor, ByVal AbsoluteY)
        0x41 -> Math (Xor, ByVal XIndirect)
        0x51 -> Math (Xor, ByVal IndirectY)
        0x69 -> Math (Adc, Immediate)
        0x65 -> Math (Adc, ByVal ZeroPage)
        0x75 -> Math (Adc, ByVal ZeroPageX)
        0x6D -> Math (Adc, ByVal Absolute)
        0x7D -> Math (Adc, ByVal AbsoluteX)
        0x79 -> Math (Adc, ByVal AbsoluteY)
        0x61 -> Math (Adc, ByVal XIndirect)
        0x71 -> Math (Adc, ByVal IndirectY)
        0xE9 -> Math (Sbc, Immediate)
        0xE5 -> Math (Sbc, ByVal ZeroPage)
        0xF5 -> Math (Sbc, ByVal ZeroPageX)
        0xED -> Math (Sbc, ByVal Absolute)
        0xFD -> Math (Sbc, ByVal AbsoluteX)
        0xF9 -> Math (Sbc, ByVal AbsoluteY)
        0xE1 -> Math (Sbc, ByVal XIndirect)
        0xF1 -> Math (Sbc, ByVal IndirectY)
        0xC9 -> Math (Compare A, Immediate)
        0xC5 -> Math (Compare A, ByVal ZeroPage)
        0xD5 -> Math (Compare A, ByVal ZeroPageX)
        0xCD -> Math (Compare A, ByVal Absolute)
        0xDD -> Math (Compare A, ByVal AbsoluteX)
        0xD9 -> Math (Compare A, ByVal AbsoluteY)
        0xC1 -> Math (Compare A, ByVal XIndirect)
        0xD1 -> Math (Compare A, ByVal IndirectY)
        0xE0 -> Math (Compare X, Immediate)
        0xE4 -> Math (Compare X, ByVal ZeroPage)
        0xEC -> Math (Compare X, ByVal Absolute)
        0xC0 -> Math (Compare Y, Immediate)
        0xC4 -> Math (Compare Y, ByVal ZeroPage)
        0xCC -> Math (Compare Y, ByVal Absolute)
        0xCA -> Math (Dec, Implied X)
        0x88 -> Math (Dec, Implied Y)
        0xC6 -> Math (Dec, Inplace ZeroPage)
        0xD6 -> Math (Dec, Inplace ZeroPageX)
        0xCE -> Math (Dec, Inplace Absolute)
        0xDE -> Math (Dec, Inplace AbsoluteX)
        0xE8 -> Math (Inc, Implied X)
        0xC8 -> Math (Inc, Implied Y)
        0xE6 -> Math (Inc, Inplace ZeroPage)
        0xF6 -> Math (Inc, Inplace ZeroPageX)
        0xEE -> Math (Inc, Inplace Absolute)
        0xFE -> Math (Inc, Inplace AbsoluteX)
        0x0A -> Math (Asl, Implied A)
        0x06 -> Math (Asl, Inplace ZeroPage)
        0x16 -> Math (Asl, Inplace ZeroPageX)
        0x0E -> Math (Asl, Inplace Absolute)
        0x1E -> Math (Asl, Inplace AbsoluteX)
        0x4A -> Math (Lsr, Implied A)
        0x46 -> Math (Lsr, Inplace ZeroPage)
        0x56 -> Math (Lsr, Inplace ZeroPageX)
        0x4E -> Math (Lsr, Inplace Absolute)
        0x5E -> Math (Lsr, Inplace AbsoluteX)
        0x2A -> Math (RotateLeft, Implied A)
        0x26 -> Math (RotateLeft, Inplace ZeroPage)
        0x36 -> Math (RotateLeft, Inplace ZeroPageX)
        0x2E -> Math (RotateLeft, Inplace Absolute)
        0x3E -> Math (RotateLeft, Inplace AbsoluteX)
        0x6A -> Math (RotateRight, Implied A)
        0x66 -> Math (RotateRight, Inplace ZeroPage)
        0x76 -> Math (RotateRight, Inplace ZeroPageX)
        0x6E -> Math (RotateRight, Inplace Absolute)
        0x7E -> Math (RotateRight, Inplace AbsoluteX)
        0x24 -> Math (Bit, ByVal ZeroPage)
        0x2C -> Math (Bit, ByVal Absolute)
        0xA9 -> Move (Load, A, Immediate)
        0xA5 -> Move (Load, A, ByVal ZeroPage)
        0xB5 -> Move (Load, A, ByVal ZeroPageX)
        0xAD -> Move (Load, A, ByVal Absolute)
        0xBD -> Move (Load, A, ByVal AbsoluteX)
        0xB9 -> Move (Load, A, ByVal AbsoluteY)
        0xA1 -> Move (Load, A, ByVal XIndirect)
        0xB1 -> Move (Load, A, ByVal IndirectY)
        0xA2 -> Move (Load, X, Immediate)
        0xA6 -> Move (Load, X, ByVal ZeroPage)
        0xB6 -> Move (Load, X, ByVal ZeroPageY)
        0xAE -> Move (Load, X, ByVal Absolute)
        0xBE -> Move (Load, X, ByVal AbsoluteY)
        0xA0 -> Move (Load, Y, Immediate)
        0xA4 -> Move (Load, Y, ByVal ZeroPage)
        0xB4 -> Move (Load, Y, ByVal ZeroPageY)
        0xAC -> Move (Load, Y, ByVal Absolute)
        0xBC -> Move (Load, Y, ByVal AbsoluteY)
        0x85 -> Move (Store, A, ByRef ZeroPage)
        0x95 -> Move (Store, A, ByRef ZeroPageX)
        0x8D -> Move (Store, A, ByRef Absolute)
        0x9D -> Move (Store, A, ByRef AbsoluteX)
        0x99 -> Move (Store, A, ByRef AbsoluteY)
        0x81 -> Move (Store, A, ByRef XIndirect)
        0x91 -> Move (Store, A, ByRef IndirectY)
        0x86 -> Move (Store, X, ByRef ZeroPage)
        0x96 -> Move (Store, X, ByRef ZeroPageY)
        0x8E -> Move (Store, X, ByRef Absolute)
        0x84 -> Move (Store, Y, ByRef ZeroPage)
        0x94 -> Move (Store, Y, ByRef ZeroPageX)
        0x8C -> Move (Store, Y, ByRef Absolute)
        0xAA -> Move (Transfer, A, Implied X)
        0x8A -> Move (Transfer, X, Implied A)
        0xA8 -> Move (Transfer, A, Implied Y)
        0x98 -> Move (Transfer, Y, Implied A)
        0x9A -> Move (Transfer, SP, Implied X)
        0xBA -> Move (Transfer, X, Implied SP)
        0x18 -> Modify (Status, Carry, Bool.false)
        0x38 -> Modify (Status, Carry, Bool.true)
        0x58 -> Modify (Status, InterruptDisable, Bool.false)
        0x78 -> Modify (Status, InterruptDisable, Bool.true)
        0xB8 -> Modify (Status, Overflow, Bool.false)
        0xD8 -> Modify (Status, DecimalMode, Bool.false)
        0xF8 -> Modify (Status, DecimalMode, Bool.true)
        0x4C -> Flow (Jump, Always, ByVal Absolute) # or ByRef?
        0x6C -> Flow (Jump, Always, Indirect) # ReadMemAddr Addr
        0x20 -> Flow (Call, Always, ByVal Absolute)
        0x10 -> Flow (Branch, On (Negative, Bool.false), Relative)
        0x30 -> Flow (Branch, On (Negative, Bool.true), Relative)
        0x50 -> Flow (Branch, On (Overflow, Bool.false), Relative)
        0x70 -> Flow (Branch, On (Overflow, Bool.true), Relative)
        0x90 -> Flow (Branch, On (Carry, Bool.false), Relative)
        0xB0 -> Flow (Branch, On (Carry, Bool.true), Relative)
        0xD0 -> Flow (Branch, On (Zero, Bool.false), Relative)
        0xF0 -> Flow (Branch, On (Zero, Bool.true), Relative)
        0x00 -> Misc (ForceInterrupt, Implied None)
        0x60 -> Misc (ReturnFromSubroutine, Implied None)
        0x40 -> Misc (ReturnFromInterrupt, Implied None)
        0x48 -> Stack (Push, Implied A)
        0x08 -> Stack (Push, Implied Status)
        0x68 -> Stack (Pop, Implied A)
        0x28 -> Stack (Pop, Implied Status)
        _ -> Unknown

# Logical and Arithmetic Operations
#
# And [AND] (Logical AND with Asccumulator)
# Performs a bitwise AND operation between the accumulator and a memory location. The result is stored in the accumulator.
# Example: AND $44 (AND the accumulator with the value at memory location $44).
#
# Or [ORA] (Logical Inclusive OR with Accumulator)
# Performs a bitwise OR operation between the accumulator and a memory location. The result is stored in the accumulator.
# Example: ORA $44 (OR the accumulator with the value at memory location $44).
#
# Xor [EOR] (Logical Exclusive OR with Accumulator)
# Performs a bitwise XOR operation between the accumulator and a memory location. The result is stored in the accumulator.
# Example: EOR $44 (XOR the accumulator with the value at memory location $44).
#
# Adc [ADC] (Add with Carry)
# Adds the value in a memory location to the accumulator, along with the carry flag. The result is stored in the accumulator.
# Example: ADC $44 (Add the value at memory location $44 to the accumulator, including the carry flag).
#
# Sbc [SBC] (Subtract with Carry)
# Subtracts the value in a memory location from the accumulator, along with the inverse of the carry flag. The result is stored in the accumulator.
# Example: SBC $44 (Subtract the value at memory location $44 from the accumulator, including the carry flag).
#
# Compare [CMP, CPX, CPY] (Compare with Accumulator, X Register or Y Register)
# Compares the value in the accumulator, X register or Y register with a memory location. It sets the processor's status flags based on the result.
# **but does not store the result.** TODO: investigate this
# Example: CMP $44 (Compare the accumulator with the value at memory location $44).
# Example: CPX $44 (Compare the X register with the value at memory location $44).
# Example: CPY $44 (Compare the Y register with the value at memory location $44).
#
# Increment [INC, INX, INY]
# Increases the value stored in a memory location, X register or Y register by one.
# Example: INC $44 (Increment the value at memory location $44 by one).
# Example: INX (Increment the X register by one).
# Example: INY (Increment the Y register by one).
#
# Decrement [DEC, DEX, DEY]
# Decreases the value stored in a memory location, X register or Y register by one.
# Example: DEC $44 (Decrement the value at memory location $44 by one).
# Example: DEX (Decrement the X register by one).
# Example: DEY (Decrement the Y register by one).

# Shift and Rotate Operations:
#
# Asl [ASL] (Arithmetic Shift Left)
# Shifts all bits in the accumulator or a memory location one bit to the left.
# The leftmost bit is shifted into the carry flag, and a 0 is shifted into the rightmost bit.
# Example: ASL $44 (Shift the value at memory location $44 one bit to the left).
#
# Lsr [LSR] (Logical Shift Right)
# Shifts all bits in the accumulator or a memory location one bit to the right.
# The rightmost bit is shifted into the carry flag, and a 0 is shifted into the leftmost bit.
# Example: LSR $44 (Shift the value at memory location $44 one bit to the right).
#
# ROL (Rotate Left): Shifts all bits in the accumulator or a memory location one bit to the left, with the carry flag being rotated into the rightmost bit and the leftmost bit being rotated into the carry flag.
# ROR (Rotate Right): Shifts all bits in the accumulator or a memory location one bit to the right, with the carry flag being rotated into the leftmost bit and the rightmost bit being rotated into the carry flag.
# Example: ROL $44 (Rotate the value at memory location $44 one bit to the left).
# Example: ROR $44 (Rotate the value at memory location $44 one bit to the right).

# Load (LDA, LDX, LDY): Load a value from memory into a register (accumulator, X, or Y).
# Store (STA, STX, STY): Store the value from a register into a memory location.
# Transfer (TAX, TXA, TAY, TYA, TSX, TXS): Transfer values between registers or between the stack pointer and a register.
# Stack (PLA, PHA, PLP, PHP): Perform stack operations, either pushing values onto the stack or pulling them from the stack.

# Branch (BPL, BMI, BVC, BVS, BCC, BCS, BNE, BEQ): Conditional branch instructions that alter program flow based on the status flags (e.g., positive/negative, overflow, carry, zero). They determine whether to jump to a different location based on specific conditions.
# Control Flow (BRK, RTI, JSR, RTS, JMP): Control the flow of execution by forcing interrupts (BRK), returning from interrupts (RTI), jumping to subroutines (JSR), returning from subroutines (RTS), and performing unconditional jumps (JMP).
# Bit Manipulation (BIT): Tests specific bits in memory against the accumulator, affecting the zero, negative, and overflow flags based on the result.
# Flag (CLC, SEC, CLD, SED, CLI, SEI, CLV): Modify specific status flags: carry (CLC, SEC), decimal mode (CLD, SED), interrupt disable (CLI, SEI), and overflow (CLV), controlling how the CPU interprets data and responds to interrupts.
# Miscellaneous (NOP): Performs no operation, simply advancing the program counter. Used for timing or code alignment.

## 1. Logical and Arithmetic Operations
# Bitwise Operations:
# Or (ORA) - Logical OR with Accumulator
# And (AND) - Logical AND with Accumulator
# Xor (EOR) - Exclusive OR with Accumulator
# Arithmetic Operations:
# Adc - Add with Carry
# Sbc - Subtract with Borrow (Carry - 1)
# Comparison Operations:
# Compare [A, X, Y] - Compare with Accumulator, X, or Y registers (CMP, CPX, CPY)
# Increment/Decrement Operations:
# Dec (DEC) - Decrement Memory
# Inc (INC) - Increment Memory
# DEX, DEY - Decrement X, Y Registers
# INX, INY - Increment X, Y Registers
# Shift and Rotate Operations:
# Asl (ASL) - Arithmetic Shift Left
# Lsr (LSR) - Logical Shift Right
# RotateLeft (ROL) - Rotate Left through Carry
# RotateRight (ROR) - Rotate Right through Carry
# 2. Data Movement Operations
# Load [A, X, Y] - Load into Accumulator, X, or Y registers (LDA, LDX, LDY)
# Store [A, X, Y] - Store Accumulator, X, or Y registers (STA, STX, STY)
# Transfer [A, SP, X, Y] - Transfer between registers (e.g., TXA, TAY, TSX, TXS)
# 3. Flags
# Modify - Set or clear specific flags (SEC, CLC, SEI, CLI, CLV, SED, CLD)
# 4. Stack Operations
# Push - Push registers onto the stack (PHA, PHP)
# Pop (Pull) - Pull registers from the stack (PLA, PLP)
# 5. Control Flow
# Jump - Unconditional jump (JMP)
# Call (JumpToSubroutine) - Call subroutine (JSR)
# Branch - Conditional relative jumps (BNE, BEQ, BPL, BMI, BVC, BVS, BCC, BCS)
# Return Operations:
# ReturnFromSubroutine - Return from subroutine (RTS)
# ReturnFromInterrupt - Return from interrupt (RTI)
# Miscellaneous Control:
# ForceInterrupt - Trigger a software interrupt (BRK)
# 6. Bit Manipulation
# Bit - Test bits in memory and set flags (BIT)
# 7. No Operation and Miscellaneous
# No Operation (NOP):
# NOP - No operation; does nothing but consumes cycles.
# Unknown/Illegal Instructions:
# Illegal Ops: Unofficial instructions not part of the standard 6502 set, often used in optimization tricks on specific hardware.
