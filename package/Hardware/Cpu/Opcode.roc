# 6502
module []

## Addressing modes
## imp (Implied): The operand is implied by the instruction itself. No additional data is needed. Example: CLC (Clear Carry Flag).
## imm (Immediate): The operand is a constant value provided directly in the instruction. Example: LDA #$10 (Load the accumulator with the value $10).
## zp (Zero Page): The operand is an 8-bit address within the first 256 bytes of memory (zero page). This allows for faster and shorter instructions. Example: LDA $00 (Load the accumulator with the value at memory location $00).
## zpx (Zero Page Indexed with X): The operand is located at an address within the zero page, but the address is calculated by adding the value of the X register to an 8-bit base address. Example: LDA $10,X (Load the accumulator with the value at memory location $10 + X).
## zpy (Zero Page Indexed with Y): Similar to zpx, but the Y register is used instead of X. Example: LDA $10,Y (Load the accumulator with the value at memory location $10 + Y).
## izx (Indirect Indexed with X): The operand's address is calculated by first adding the X register to a zero-page address, then using the value at this address as a pointer to the final memory location. Example: LDA ($10,X).
## izy (Indirect Indexed with Y): The operand's address is determined by first looking up a zero-page address, then adding the Y register to this address to get the final memory location. Example: LDA ($10),Y.
## abs (Absolute): The operand is a 16-bit memory address provided directly in the instruction. Example: LDA $1234 (Load the accumulator with the value at memory location $1234).
## abx (Absolute Indexed with X): The operand's address is calculated by adding the X register to a 16-bit base address. Example: LDA $1000,X (Load the accumulator with the value at memory location $1000 + X).
## aby (Absolute Indexed with Y): Similar to abx, but the Y register is used instead of X. Example: LDA $1000,Y (Load the accumulator with the value at memory location $1000 + Y).
## ind (Indirect): The operand's address is stored at a specified memory location. This is typically used for jump instructions. Example: JMP ($1000) (Jump to the address stored at memory location $1000).
## rel (Relative): The operand is a signed 8-bit offset used for branch instructions, which is added to the program counter (PC) to determine the jump target. Example: BEQ $10 (Branch if equal to the location PC + $10).
Addressing : [Implied, Immediate, ZeroPage [Base, X, Y], Absolute [Base, X, Y], Indirect [Base, X, Y], Relative]

Lookup cycle : [
    ADC [ABS, ABX, ABY, IMM, IZX, IZY, ZP0, ZPX] cycle,
    AND [ABS, ABX, ABY, IMM, IZX, IZY, ZP0, ZPX] cycle,
    ASL [ABS, ABX, IMP, ZP0, ZPX] cycle,
    BCC [REL] cycle,
    BCS [REL] cycle,
    BEQ [REL] cycle,
    BIT [ABS, ZP0] cycle,
    BMI [REL] cycle,
    BNE [REL] cycle,
    BPL [REL] cycle,
    BRK [IMM] cycle,
    BVC [REL] cycle,
    BVS [REL] cycle,
    CLC [IMP] cycle,
    CLD [IMP] cycle,
    CLI [IMP] cycle,
    CLV [IMP] cycle,
    CMP [ABS, ABX, ABY, IMM, IZX, IZY, ZP0, ZPX] cycle,
    CPX [ABS, IMM, ZP0] cycle,
    CPY [ABS, IMM, ZP0] cycle,
    DEC [ABS, ABX, ZP0, ZPX] cycle,
    DEX [IMP] cycle,
    DEY [IMP] cycle,
    EOR [ABS, ABX, ABY, IMM, IZX, IZY, ZP0, ZPX] cycle,
    INC [ABS, ABX, ZP0, ZPX] cycle,
    INX [IMP] cycle,
    INY [IMP] cycle,
    JMP [ABS, IND] cycle,
    JSR [ABS] cycle,
    LDA [ABS, ABX, ABY, IMM, IZX, IZY, ZP0, ZPX] cycle,
    LDX [ABS, ABY, IMM, ZP0, ZPY] cycle,
    LDY [ABS, ABX, IMM, ZP0, ZPX] cycle,
    LSR [ABS, ABX, IMP, ZP0, ZPX] cycle,
    NOP [IMP] cycle,
    ORA [ABS, ABX, ABY, IMM, IZX, IZY, ZP0, ZPX] cycle,
    PHA [IMP] cycle,
    PHP [IMP] cycle,
    PLA [IMP] cycle,
    PLP [IMP] cycle,
    ROL [ABS, ABX, IMP, ZP0, ZPX] cycle,
    ROR [ABS, ABX, IMP, ZP0, ZPX] cycle,
    RTI [IMP] cycle,
    RTS [IMP] cycle,
    SBC [ABS, ABX, ABY, IMM, IMP, IZX, IZY, ZP0, ZPX] cycle,
    SEC [IMP] cycle,
    SED [IMP] cycle,
    SEI [IMP] cycle,
    STA [ABS, ABX, ABY, IZX, IZY, ZP0, ZPX] cycle,
    STX [ABS, ZP0, ZPY] cycle,
    STY [ABS, ZP0, ZPX] cycle,
    TAX [IMP] cycle,
    TAY [IMP] cycle,
    TSX [IMP] cycle,
    TXA [IMP] cycle,
    TXS [IMP] cycle,
    TYA [IMP] cycle,
    Illegal [IMP] cycle,
    Unknown,
]

lookup : U8 -> Lookup U8
lookup = \byte ->
    when byte is
        0x00 -> BRK IMM 7
        0x01 -> ORA IZX 6
        0x02 -> Illegal IMP 2
        0x03 -> Illegal IMP 8
        0x04 -> Illegal IMP 3
        0x05 -> ORA ZP0 3
        0x06 -> ASL ZP0 5
        0x07 -> Illegal IMP 5
        0x08 -> PHP IMP 3
        0x09 -> ORA IMM 2
        0x0A -> ASL IMP 2
        0x0B -> Illegal IMP 2
        0x0C -> Illegal IMP 4
        0x0D -> ORA ABS 4
        0x0E -> ASL ABS 6
        0x0F -> Illegal IMP 6
        0x10 -> BPL REL 2
        0x11 -> ORA IZY 5
        0x12 -> Illegal IMP 2
        0x13 -> Illegal IMP 8
        0x14 -> NOP IMP 4
        0x15 -> ORA ZPX 4
        0x16 -> ASL ZPX 6
        0x17 -> Illegal IMP 6
        0x18 -> CLC IMP 2
        0x19 -> ORA ABY 4
        0x1A -> NOP IMP 2
        0x1B -> Illegal IMP 7
        0x1C -> NOP IMP 4
        0x1D -> ORA ABX 4
        0x1E -> ASL ABX 7
        0x1F -> Illegal IMP 7
        0x20 -> JSR ABS 6
        0x21 -> AND IZX 6
        0x22 -> Illegal IMP 2
        0x23 -> Illegal IMP 8
        0x24 -> BIT ZP0 3
        0x25 -> AND ZP0 3
        0x26 -> ROL ZP0 5
        0x27 -> Illegal IMP 5
        0x28 -> PLP IMP 4
        0x29 -> AND IMM 2
        0x2A -> ROL IMP 2
        0x2B -> Illegal IMP 2
        0x2C -> BIT ABS 4
        0x2D -> AND ABS 4
        0x2E -> ROL ABS 6
        0x2F -> Illegal IMP 6
        0x30 -> BMI REL 2
        0x31 -> AND IZY 5
        0x32 -> Illegal IMP 2
        0x33 -> Illegal IMP 8
        0x34 -> NOP IMP 4
        0x35 -> AND ZPX 4
        0x36 -> ROL ZPX 6
        0x37 -> Illegal IMP 6
        0x38 -> SEC IMP 2
        0x39 -> AND ABY 4
        0x3A -> NOP IMP 2
        0x3B -> Illegal IMP 7
        0x3C -> NOP IMP 4
        0x3D -> AND ABX 4
        0x3E -> ROL ABX 7
        0x3F -> Illegal IMP 7
        0x40 -> RTI IMP 6
        0x41 -> EOR IZX 6
        0x42 -> Illegal IMP 2
        0x43 -> Illegal IMP 8
        0x44 -> NOP IMP 3
        0x45 -> EOR ZP0 3
        0x46 -> LSR ZP0 5
        0x47 -> Illegal IMP 5
        0x48 -> PHA IMP 3
        0x49 -> EOR IMM 2
        0x4A -> LSR IMP 2
        0x4B -> Illegal IMP 2
        0x4C -> JMP ABS 3
        0x4D -> EOR ABS 4
        0x4E -> LSR ABS 6
        0x4F -> Illegal IMP 6
        0x50 -> BVC REL 2
        0x51 -> EOR IZY 5
        0x52 -> Illegal IMP 2
        0x53 -> Illegal IMP 8
        0x54 -> NOP IMP 4
        0x55 -> EOR ZPX 4
        0x56 -> LSR ZPX 6
        0x57 -> Illegal IMP 6
        0x58 -> CLI IMP 2
        0x59 -> EOR ABY 4
        0x5A -> NOP IMP 2
        0x5B -> Illegal IMP 7
        0x5C -> NOP IMP 4
        0x5D -> EOR ABX 4
        0x5E -> LSR ABX 7
        0x5F -> Illegal IMP 7
        0x60 -> RTS IMP 6
        0x61 -> ADC IZX 6
        0x62 -> Illegal IMP 2
        0x63 -> Illegal IMP 8
        0x64 -> NOP IMP 3
        0x65 -> ADC ZP0 3
        0x66 -> ROR ZP0 5
        0x67 -> Illegal IMP 5
        0x68 -> PLA IMP 4
        0x69 -> ADC IMM 2
        0x6A -> ROR IMP 2
        0x6B -> Illegal IMP 2
        0x6C -> JMP IND 5
        0x6D -> ADC ABS 4
        0x6E -> ROR ABS 6
        0x6F -> Illegal IMP 6
        0x70 -> BVS REL 2
        0x71 -> ADC IZY 5
        0x72 -> Illegal IMP 2
        0x73 -> Illegal IMP 8
        0x74 -> NOP IMP 4
        0x75 -> ADC ZPX 4
        0x76 -> ROR ZPX 6
        0x77 -> Illegal IMP 6
        0x78 -> SEI IMP 2
        0x79 -> ADC ABY 4
        0x7A -> NOP IMP 2
        0x7B -> Illegal IMP 7
        0x7C -> NOP IMP 4
        0x7D -> ADC ABX 4
        0x7E -> ROR ABX 7
        0x7F -> Illegal IMP 7
        0x80 -> NOP IMP 2
        0x81 -> STA IZX 6
        0x82 -> NOP IMP 2
        0x83 -> Illegal IMP 6
        0x84 -> STY ZP0 3
        0x85 -> STA ZP0 3
        0x86 -> STX ZP0 3
        0x87 -> Illegal IMP 3
        0x88 -> DEY IMP 2
        0x89 -> NOP IMP 2
        0x8A -> TXA IMP 2
        0x8B -> Illegal IMP 2
        0x8C -> STY ABS 4
        0x8D -> STA ABS 4
        0x8E -> STX ABS 4
        0x8F -> Illegal IMP 4
        0x90 -> BCC REL 2
        0x91 -> STA IZY 6
        0x92 -> Illegal IMP 2
        0x93 -> Illegal IMP 6
        0x94 -> STY ZPX 4
        0x95 -> STA ZPX 4
        0x96 -> STX ZPY 4
        0x97 -> Illegal IMP 4
        0x98 -> TYA IMP 2
        0x99 -> STA ABY 5
        0x9A -> TXS IMP 2
        0x9B -> Illegal IMP 5
        0x9C -> NOP IMP 5
        0x9D -> STA ABX 5
        0x9E -> Illegal IMP 5
        0x9F -> Illegal IMP 5
        0xA0 -> LDY IMM 2
        0xA1 -> LDA IZX 6
        0xA2 -> LDX IMM 2
        0xA3 -> Illegal IMP 6
        0xA4 -> LDY ZP0 3
        0xA5 -> LDA ZP0 3
        0xA6 -> LDX ZP0 3
        0xA7 -> Illegal IMP 3
        0xA8 -> TAY IMP 2
        0xA9 -> LDA IMM 2
        0xAA -> TAX IMP 2
        0xAB -> Illegal IMP 2
        0xAC -> LDY ABS 4
        0xAD -> LDA ABS 4
        0xAE -> LDX ABS 4
        0xAF -> Illegal IMP 4
        0xB0 -> BCS REL 2
        0xB1 -> LDA IZY 5
        0xB2 -> Illegal IMP 2
        0xB3 -> Illegal IMP 5
        0xB4 -> LDY ZPX 4
        0xB5 -> LDA ZPX 4
        0xB6 -> LDX ZPY 4
        0xB7 -> Illegal IMP 4
        0xB8 -> CLV IMP 2
        0xB9 -> LDA ABY 4
        0xBA -> TSX IMP 2
        0xBB -> Illegal IMP 4
        0xBC -> LDY ABX 4
        0xBD -> LDA ABX 4
        0xBE -> LDX ABY 4
        0xBF -> Illegal IMP 4
        0xC0 -> CPY IMM 2
        0xC1 -> CMP IZX 6
        0xC2 -> NOP IMP 2
        0xC3 -> Illegal IMP 8
        0xC4 -> CPY ZP0 3
        0xC5 -> CMP ZP0 3
        0xC6 -> DEC ZP0 5
        0xC7 -> Illegal IMP 5
        0xC8 -> INY IMP 2
        0xC9 -> CMP IMM 2
        0xCA -> DEX IMP 2
        0xCB -> Illegal IMP 2
        0xCC -> CPY ABS 4
        0xCD -> CMP ABS 4
        0xCE -> DEC ABS 6
        0xCF -> Illegal IMP 6
        0xD0 -> BNE REL 2
        0xD1 -> CMP IZY 5
        0xD2 -> Illegal IMP 2
        0xD3 -> Illegal IMP 8
        0xD4 -> NOP IMP 4
        0xD5 -> CMP ZPX 4
        0xD6 -> DEC ZPX 6
        0xD7 -> Illegal IMP 6
        0xD8 -> CLD IMP 2
        0xD9 -> CMP ABY 4
        0xDA -> NOP IMP 2
        0xDB -> Illegal IMP 7
        0xDC -> NOP IMP 4
        0xDD -> CMP ABX 4
        0xDE -> DEC ABX 7
        0xDF -> Illegal IMP 7
        0xE0 -> CPX IMM 2
        0xE1 -> SBC IZX 6
        0xE2 -> NOP IMP 2
        0xE3 -> Illegal IMP 8
        0xE4 -> CPX ZP0 3
        0xE5 -> SBC ZP0 3
        0xE6 -> INC ZP0 5
        0xE7 -> Illegal IMP 5
        0xE8 -> INX IMP 2
        0xE9 -> SBC IMM 2
        0xEA -> NOP IMP 2
        0xEB -> SBC IMP 2
        0xEC -> CPX ABS 4
        0xED -> SBC ABS 4
        0xEE -> INC ABS 6
        0xEF -> Illegal IMP 6
        0xF0 -> BEQ REL 2
        0xF1 -> SBC IZY 5
        0xF2 -> Illegal IMP 2
        0xF3 -> Illegal IMP 8
        0xF4 -> NOP IMP 4
        0xF5 -> SBC ZPX 4
        0xF6 -> INC ZPX 6
        0xF7 -> Illegal IMP 6
        0xF8 -> SED IMP 2
        0xF9 -> SBC ABY 4
        0xFA -> NOP IMP 2
        0xFB -> Illegal IMP 7
        0xFC -> NOP IMP 4
        0xFD -> SBC ABX 4
        0xFE -> INC ABX 7
        0xFF -> Illegal IMP 7
        _ -> Unknown
