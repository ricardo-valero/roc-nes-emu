# 6502
module []

## Addressing modes
## Implied (IMP): The operand is implied by the instruction itself. No additional data is needed. Example: CLC (Clear Carry Flag).
## Immediate (IMM): The operand is a constant value provided directly in the instruction. Example: LDA #$10 (Load the accumulator with the value $10).
## Zero Page (ZP0, ZPX, ZPY): The operand is an 8-bit address within the first 256 bytes of memory (zero page). This allows for faster and shorter instructions. Example: LDA $00 (Load the accumulator with the value at memory location $00).
## - The operand is located at an address within the zero page, but the address is calculated by adding the value of the X register to an 8-bit base address. Example: LDA $10,X (Load the accumulator with the value at memory location $10 + X).
## - Similar to zpx, but the Y register is used instead of X. Example: LDA $10,Y (Load the accumulator with the value at memory location $10 + Y).
## Indirect (IND, IZX, IZY): The operand's address is stored at a specified memory location. This is typically used for jump instructions. Example: JMP ($1000) (Jump to the address stored at memory location $1000).
## - The operand's address is calculated by first adding the X register to a zero-page address, then using the value at this address as a pointer to the final memory location. Example: LDA ($10,X).
## TODO: THIS IS DIFFERENT - The operand's address is determined by first looking up a zero-page address, then adding the Y register to this address to get the final memory location. Example: LDA ($10),Y.
## Absolute (ABS, ABX, ABY): The operand is a 16-bit memory address provided directly in the instruction. Example: LDA $1234 (Load the accumulator with the value at memory location $1234).
## - The operand's address is calculated by adding the X register to a 16-bit base address. Example: LDA $1000,X (Load the accumulator with the value at memory location $1000 + X).
## - Similar to abx, but the Y register is used instead of X. Example: LDA $1000,Y (Load the accumulator with the value at memory location $1000 + Y).
## Relative (REL): The operand is a signed 8-bit offset used for branch instructions, which is added to the program counter (PC) to determine the jump target. Example: BEQ $10 (Branch if equal to the location PC + $10).

AddressingMode : [Implied, Immediate, ZeroPage [Base, X, Y], Absolute [Base, X, Y], Indirect [Base, X, Y], Relative]

Lookup cycle : [
    # Logical and arithmetic
    ORA [Absolute, AbsoluteX, AbsoluteY, Immediate, XIndirect, IndirectY, ZeroPage, ZeroPageX] cycle,
    AND [Absolute, AbsoluteX, AbsoluteY, Immediate, XIndirect, IndirectY, ZeroPage, ZeroPageX] cycle,
    EOR [Absolute, AbsoluteX, AbsoluteY, Immediate, XIndirect, IndirectY, ZeroPage, ZeroPageX] cycle,
    ADC [Absolute, AbsoluteX, AbsoluteY, Immediate, XIndirect, IndirectY, ZeroPage, ZeroPageX] cycle,
    SBC [Absolute, AbsoluteX, AbsoluteY, Immediate, XIndirect, IndirectY, ZeroPage, ZeroPageX] cycle,
    CMP [Absolute, AbsoluteX, AbsoluteY, Immediate, XIndirect, IndirectY, ZeroPage, ZeroPageX] cycle,
    LDA [Absolute, AbsoluteX, AbsoluteY, Immediate, XIndirect, IndirectY, ZeroPage, ZeroPageX] cycle,
    # Move
    ASL [Absolute, AbsoluteX, Implied, ZeroPage, ZeroPageX] cycle,
    DEC [Absolute, AbsoluteX, ZeroPage, ZeroPageX] cycle,
    INC [Absolute, AbsoluteX, ZeroPage, ZeroPageX] cycle,
    LDY [Absolute, AbsoluteX, Immediate, ZeroPage, ZeroPageX] cycle,
    LSR [Absolute, AbsoluteX, Implied, ZeroPage, ZeroPageX] cycle,
    ROL [Absolute, AbsoluteX, Implied, ZeroPage, ZeroPageX] cycle,
    ROR [Absolute, AbsoluteX, Implied, ZeroPage, ZeroPageX] cycle,
    LDX [Absolute, AbsoluteY, Immediate, ZeroPage, ZeroPageY] cycle,
    # Jump
    #
    # Flag (branch on condition)
    # Flag (set or clear)
    #
    # Other
    BCC [Relative] cycle,
    BCS [Relative] cycle,
    BEQ [Relative] cycle,
    BIT [Absolute, ZeroPage] cycle,
    BMI [Relative] cycle,
    BNE [Relative] cycle,
    BPL [Relative] cycle,
    BRK [Immediate] cycle,
    BVC [Relative] cycle,
    BVS [Relative] cycle,
    CLC [Implied] cycle,
    CLD [Implied] cycle,
    CLI [Implied] cycle,
    CLV [Implied] cycle,
    CPX [Absolute, Immediate, ZeroPage] cycle,
    CPY [Absolute, Immediate, ZeroPage] cycle,
    DEX [Implied] cycle,
    DEY [Implied] cycle,
    INX [Implied] cycle,
    INY [Implied] cycle,
    JMP [Absolute, Indirect] cycle,
    JSR [Absolute] cycle,
    NOP [Implied] cycle,
    PHA [Implied] cycle,
    PHP [Implied] cycle,
    PLA [Implied] cycle,
    PLP [Implied] cycle,
    RTI [Implied] cycle,
    RTS [Implied] cycle,
    SEC [Implied] cycle,
    SED [Implied] cycle,
    SEI [Implied] cycle,
    STA [Absolute, AbsoluteX, AbsoluteY, XIndirect, IndirectY, ZeroPage, ZeroPageX] cycle,
    STX [Absolute, ZeroPage, ZeroPageY] cycle,
    STY [Absolute, ZeroPage, ZeroPageX] cycle,
    TAX [Implied] cycle,
    TAY [Implied] cycle,
    TSX [Implied] cycle,
    TXA [Implied] cycle,
    TXS [Implied] cycle,
    TYA [Implied] cycle,
    # Nop
    # Illegal
    Illegal [Implied] cycle,
    Unknown,
]

lookup : U8 -> Lookup U8
lookup = \byte ->
    when byte is
        # Logical and arithmetic
        0x01 -> ORA XIndirect 6
        0x05 -> ORA ZeroPage 3
        0x09 -> ORA Immediate 2
        0x0D -> ORA Absolute 4
        0x11 -> ORA IndirectY 5
        0x15 -> ORA ZeroPageX 4
        0x19 -> ORA AbsoluteY 4
        0x1D -> ORA AbsoluteX 4
        0x21 -> AND XIndirect 6
        0x25 -> AND ZeroPage 3
        0x29 -> AND Immediate 2
        0x2D -> AND Absolute 4
        0x31 -> AND IndirectY 5
        0x35 -> AND ZeroPageX 4
        0x39 -> AND AbsoluteY 4
        0x3D -> AND AbsoluteX 4
        0x41 -> EOR XIndirect 6
        0x45 -> EOR ZeroPage 3
        0x49 -> EOR Immediate 2
        0x4D -> EOR Absolute 4
        0x51 -> EOR IndirectY 5
        0x55 -> EOR ZeroPageX 4
        0x59 -> EOR AbsoluteY 4
        0x5D -> EOR AbsoluteX 4
        0x61 -> ADC XIndirect 6
        0x65 -> ADC ZeroPage 3
        0x69 -> ADC Immediate 2
        0x6D -> ADC Absolute 4
        0x71 -> ADC IndirectY 5
        0x75 -> ADC ZeroPageX 4
        0x79 -> ADC AbsoluteY 4
        0x7D -> ADC AbsoluteX 4
        0xE1 -> SBC XIndirect 6
        0xE5 -> SBC ZeroPage 3
        0xE9 -> SBC Immediate 2
        0xED -> SBC Absolute 4
        0xF1 -> SBC IndirectY 5
        0xF5 -> SBC ZeroPageX 4
        0xF9 -> SBC AbsoluteY 4
        0xFD -> SBC AbsoluteX 4
        0xC1 -> CMP XIndirect 6
        0xC5 -> CMP ZeroPage 3
        0xC9 -> CMP Immediate 2
        0xCD -> CMP Absolute 4
        0xD1 -> CMP IndirectY 5
        0xD5 -> CMP ZeroPageX 4
        0xD9 -> CMP AbsoluteY 4
        0xDD -> CMP AbsoluteX 4
        0xE0 -> CPX Immediate 2
        0xE4 -> CPX ZeroPage 3
        0xEC -> CPX Absolute 4
        0xC0 -> CPY Immediate 2
        0xC4 -> CPY ZeroPage 3
        0xCC -> CPY Absolute 4
        0xC6 -> DEC ZeroPage 5
        0xCE -> DEC Absolute 6
        0xD6 -> DEC ZeroPageX 6
        0xDE -> DEC AbsoluteX 7
        0xCA -> DEX Implied 2
        0x88 -> DEY Implied 2
        0xE6 -> INC ZeroPage 5
        0xEE -> INC Absolute 6
        0xF6 -> INC ZeroPageX 6
        0xFE -> INC AbsoluteX 7
        0xE8 -> INX Implied 2
        0xC8 -> INY Implied 2
        0x06 -> ASL ZeroPage 5
        0x0A -> ASL Implied 2
        0x0E -> ASL Absolute 6
        0x16 -> ASL ZeroPageX 6
        0x1E -> ASL AbsoluteX 7
        0x46 -> LSR ZeroPage 5
        0x4A -> LSR Implied 2
        0x4E -> LSR Absolute 6
        0x56 -> LSR ZeroPageX 6
        0x5E -> LSR AbsoluteX 7
        0x26 -> ROL ZeroPage 5
        0x2A -> ROL Implied 2
        0x2E -> ROL Absolute 6
        0x36 -> ROL ZeroPageX 6
        0x3E -> ROL AbsoluteX 7
        0x66 -> ROR ZeroPage 5
        0x6A -> ROR Implied 2
        0x6E -> ROR Absolute 6
        0x76 -> ROR ZeroPageX 6
        0x7E -> ROR AbsoluteX 7
        # Move
        0xA1 -> LDA XIndirect 6
        0xA5 -> LDA ZeroPage 3
        0xA9 -> LDA Immediate 2
        0xAD -> LDA Absolute 4
        0xB1 -> LDA IndirectY 5
        0xB5 -> LDA ZeroPageX 4
        0xB9 -> LDA AbsoluteY 4
        0xBD -> LDA AbsoluteX 4
        0xA2 -> LDX Immediate 2
        0xA6 -> LDX ZeroPage 3
        0xAE -> LDX Absolute 4
        0xB6 -> LDX ZeroPageY 4
        0xBE -> LDX AbsoluteY 4
        0xA0 -> LDY Immediate 2
        0xA4 -> LDY ZeroPage 3
        0xAC -> LDY Absolute 4
        0xB4 -> LDY ZeroPageX 4
        0xBC -> LDY AbsoluteX 4
        0x81 -> STA XIndirect 6
        0x85 -> STA ZeroPage 3
        0x8D -> STA Absolute 4
        0x91 -> STA IndirectY 6
        0x95 -> STA ZeroPageX 4
        0x99 -> STA AbsoluteY 5
        0x9D -> STA AbsoluteX 5
        0x86 -> STX ZeroPage 3
        0x8E -> STX Absolute 4
        0x96 -> STX ZeroPageY 4
        0x84 -> STY ZeroPage 3
        0x8C -> STY Absolute 4
        0x94 -> STY ZeroPageX 4
        0x8A -> TXA Implied 2
        0xAA -> TAX Implied 2
        0x98 -> TYA Implied 2
        0xA8 -> TAY Implied 2
        0x9A -> TXS Implied 2
        0xBA -> TSX Implied 2
        0x68 -> PLA Implied 4
        0x48 -> PHA Implied 3
        0x28 -> PLP Implied 4
        0x08 -> PHP Implied 3
        # Jump
        0x4C -> JMP Absolute 3
        0x6C -> JMP Indirect 5
        0x20 -> JSR Absolute 6
        # Flag (branch on condition)
        0x10 -> BPL Relative 2
        0x30 -> BMI Relative 2
        0x50 -> BVC Relative 2
        0x70 -> BVS Relative 2
        0x90 -> BCC Relative 2
        0xB0 -> BCS Relative 2
        0xD0 -> BNE Relative 2
        0xF0 -> BEQ Relative 2
        # Flag (set or clear)
        0x18 -> CLC Implied 2
        0x38 -> SEC Implied 2
        0x58 -> CLI Implied 2
        0x78 -> SEI Implied 2
        0xB8 -> CLV Implied 2
        0xD8 -> CLD Implied 2
        0xF8 -> SED Implied 2
        # Other
        0x00 -> BRK Immediate 7
        0x40 -> RTI Implied 6
        0x60 -> RTS Implied 6
        0x24 -> BIT ZeroPage 3
        0x2C -> BIT Absolute 4
        # Nop
        0x14 -> NOP Implied 4
        0x1A -> NOP Implied 2
        0x1C -> NOP Implied 4
        0x34 -> NOP Implied 4
        0x3A -> NOP Implied 2
        0x3C -> NOP Implied 4
        0x44 -> NOP Implied 3
        0x54 -> NOP Implied 4
        0x5A -> NOP Implied 2
        0x5C -> NOP Implied 4
        0x64 -> NOP Implied 3
        0x74 -> NOP Implied 4
        0x7A -> NOP Implied 2
        0x7C -> NOP Implied 4
        0x80 -> NOP Implied 2
        0x82 -> NOP Implied 2
        0x89 -> NOP Implied 2
        0x9C -> NOP Implied 5
        0xC2 -> NOP Implied 2
        0xD4 -> NOP Implied 4
        0xDA -> NOP Implied 2
        0xDC -> NOP Implied 4
        0xE2 -> NOP Implied 2
        0xEA -> NOP Implied 2
        0xF4 -> NOP Implied 4
        0xFA -> NOP Implied 2
        0xFC -> NOP Implied 4
        # Illegal
        0x02 -> Illegal Implied 2
        0x03 -> Illegal Implied 8
        0x04 -> Illegal Implied 3
        0x07 -> Illegal Implied 5
        0x0B -> Illegal Implied 2
        0x0C -> Illegal Implied 4
        0x0F -> Illegal Implied 6
        0x12 -> Illegal Implied 2
        0x13 -> Illegal Implied 8
        0x17 -> Illegal Implied 6
        0x1B -> Illegal Implied 7
        0x1F -> Illegal Implied 7
        0x22 -> Illegal Implied 2
        0x23 -> Illegal Implied 8
        0x27 -> Illegal Implied 5
        0x2B -> Illegal Implied 2
        0x2F -> Illegal Implied 6
        0x32 -> Illegal Implied 2
        0x33 -> Illegal Implied 8
        0x37 -> Illegal Implied 6
        0x3B -> Illegal Implied 7
        0x3F -> Illegal Implied 7
        0x42 -> Illegal Implied 2
        0x43 -> Illegal Implied 8
        0x47 -> Illegal Implied 5
        0x4B -> Illegal Implied 2
        0x4F -> Illegal Implied 6
        0x52 -> Illegal Implied 2
        0x53 -> Illegal Implied 8
        0x57 -> Illegal Implied 6
        0x5B -> Illegal Implied 7
        0x5F -> Illegal Implied 7
        0x62 -> Illegal Implied 2
        0x63 -> Illegal Implied 8
        0x67 -> Illegal Implied 5
        0x6B -> Illegal Implied 2
        0x6F -> Illegal Implied 6
        0x72 -> Illegal Implied 2
        0x73 -> Illegal Implied 8
        0x77 -> Illegal Implied 6
        0x7B -> Illegal Implied 7
        0x7F -> Illegal Implied 7
        0x83 -> Illegal Implied 6
        0x87 -> Illegal Implied 3
        0x8B -> Illegal Implied 2
        0x8F -> Illegal Implied 4
        0x92 -> Illegal Implied 2
        0x93 -> Illegal Implied 6
        0x97 -> Illegal Implied 4
        0x9B -> Illegal Implied 5
        0x9E -> Illegal Implied 5
        0x9F -> Illegal Implied 5
        0xA3 -> Illegal Implied 6
        0xA7 -> Illegal Implied 3
        0xAB -> Illegal Implied 2
        0xAF -> Illegal Implied 4
        0xB2 -> Illegal Implied 2
        0xB3 -> Illegal Implied 5
        0xB7 -> Illegal Implied 4
        0xBB -> Illegal Implied 4
        0xBF -> Illegal Implied 4
        0xC3 -> Illegal Implied 8
        0xC7 -> Illegal Implied 5
        0xCB -> Illegal Implied 2
        0xCF -> Illegal Implied 6
        0xD2 -> Illegal Implied 2
        0xD3 -> Illegal Implied 8
        0xD7 -> Illegal Implied 6
        0xDB -> Illegal Implied 7
        0xDF -> Illegal Implied 7
        0xE3 -> Illegal Implied 8
        0xE7 -> Illegal Implied 5
        0xEF -> Illegal Implied 6
        0xF2 -> Illegal Implied 2
        0xF3 -> Illegal Implied 8
        0xF7 -> Illegal Implied 6
        0xFB -> Illegal Implied 7
        0xFF -> Illegal Implied 7
        _ -> Unknown
