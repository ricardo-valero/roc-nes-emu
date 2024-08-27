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
## - The operand's address is determined by first looking up a zero-page address, then adding the Y register to this address to get the final memory location. Example: LDA ($10),Y.
## Absolute (ABS, ABX, ABY): The operand is a 16-bit memory address provided directly in the instruction. Example: LDA $1234 (Load the accumulator with the value at memory location $1234).
## - The operand's address is calculated by adding the X register to a 16-bit base address. Example: LDA $1000,X (Load the accumulator with the value at memory location $1000 + X).
## - Similar to abx, but the Y register is used instead of X. Example: LDA $1000,Y (Load the accumulator with the value at memory location $1000 + Y).
## Relative (REL): The operand is a signed 8-bit offset used for branch instructions, which is added to the program counter (PC) to determine the jump target. Example: BEQ $10 (Branch if equal to the location PC + $10).
Addressing : [Implied, Immediate, ZeroPage [Base, X, Y], Absolute [Base, X, Y], Indirect [Base, X, Y], Relative]

# lookup : U8 -> Lookup U8
lookup = \byte ->
    when byte is
        # Logical and arithmetic
        0x01 -> ORA (Indirect X) 6
        0x05 -> ORA (ZeroPage Base) 3
        0x09 -> ORA Immediate 2
        0x0D -> ORA (Absolute Base) 4
        0x11 -> ORA (Indirect Y) 5
        0x15 -> ORA (ZeroPage X) 4
        0x19 -> ORA (Absolute Y) 4
        0x1D -> ORA (Absolute X) 4
        0x21 -> AND (Indirect X) 6
        0x25 -> AND (ZeroPage Base) 3
        0x29 -> AND Immediate 2
        0x2D -> AND (Absolute Base) 4
        0x31 -> AND (Indirect Y) 5
        0x35 -> AND (ZeroPage X) 4
        0x39 -> AND (Absolute Y) 4
        0x3D -> AND (Absolute X) 4
        0x41 -> EOR (Indirect X) 6
        0x45 -> EOR (ZeroPage Base) 3
        0x49 -> EOR Immediate 2
        0x4D -> EOR (Absolute Base) 4
        0x51 -> EOR (Indirect Y) 5
        0x55 -> EOR (ZeroPage X) 4
        0x59 -> EOR (Absolute Y) 4
        0x5D -> EOR (Absolute X) 4
        0x61 -> ADC (Indirect X) 6
        0x65 -> ADC (ZeroPage Base) 3
        0x69 -> ADC Immediate 2
        0x6D -> ADC (Absolute Base) 4
        0x71 -> ADC (Indirect Y) 5
        0x75 -> ADC (ZeroPage X) 4
        0x79 -> ADC (Absolute Y) 4
        0x7D -> ADC (Absolute X) 4
        0xE1 -> SBC (Indirect X) 6
        0xE5 -> SBC (ZeroPage Base) 3
        0xE9 -> SBC Immediate 2
        0xEB -> SBC Implied 2
        0xED -> SBC (Absolute Base) 4
        0xF1 -> SBC (Indirect Y) 5
        0xF5 -> SBC (ZeroPage X) 4
        0xF9 -> SBC (Absolute Y) 4
        0xFD -> SBC (Absolute X) 4
        0xC1 -> CMP (Indirect X) 6
        0xC5 -> CMP (ZeroPage Base) 3
        0xC9 -> CMP Immediate 2
        0xCD -> CMP (Absolute Base) 4
        0xD1 -> CMP (Indirect Y) 5
        0xD5 -> CMP (ZeroPage X) 4
        0xD9 -> CMP (Absolute Y) 4
        0xDD -> CMP (Absolute X) 4
        0xE0 -> CPX Immediate 2
        0xE4 -> CPX (ZeroPage Base) 3
        0xEC -> CPX (Absolute Base) 4
        0xC0 -> CPY Immediate 2
        0xC4 -> CPY (ZeroPage Base) 3
        0xCC -> CPY (Absolute Base) 4
        0xC6 -> DEC (ZeroPage Base) 5
        0xCE -> DEC (Absolute Base) 6
        0xD6 -> DEC (ZeroPage X) 6
        0xDE -> DEC (Absolute X) 7
        0xCA -> DEX Implied 2
        0x88 -> DEY Implied 2
        0xE6 -> INC (ZeroPage Base) 5
        0xEE -> INC (Absolute Base) 6
        0xF6 -> INC (ZeroPage X) 6
        0xFE -> INC (Absolute X) 7
        0xE8 -> INX Implied 2
        0xC8 -> INY Implied 2
        0x06 -> ASL (ZeroPage Base) 5
        0x0A -> ASL Implied 2
        0x0E -> ASL (Absolute Base) 6
        0x16 -> ASL (ZeroPage X) 6
        0x1E -> ASL (Absolute X) 7
        0x46 -> LSR (ZeroPage Base) 5
        0x4A -> LSR Implied 2
        0x4E -> LSR (Absolute Base) 6
        0x56 -> LSR (ZeroPage X) 6
        0x5E -> LSR (Absolute X) 7
        0x26 -> ROL (ZeroPage Base) 5
        0x2A -> ROL Implied 2
        0x2E -> ROL (Absolute Base) 6
        0x36 -> ROL (ZeroPage X) 6
        0x3E -> ROL (Absolute X) 7
        0x66 -> ROR (ZeroPage Base) 5
        0x6A -> ROR Implied 2
        0x6E -> ROR (Absolute Base) 6
        0x76 -> ROR (ZeroPage X) 6
        0x7E -> ROR (Absolute X) 7
        # Move
        0xA1 -> LDA (Indirect X) 6
        0xA5 -> LDA (ZeroPage Base) 3
        0xA9 -> LDA Immediate 2
        0xAD -> LDA (Absolute Base) 4
        0xB1 -> LDA (Indirect Y) 5
        0xB5 -> LDA (ZeroPage X) 4
        0xB9 -> LDA (Absolute Y) 4
        0xBD -> LDA (Absolute X) 4
        0xA2 -> LDX Immediate 2
        0xA6 -> LDX (ZeroPage Base) 3
        0xAE -> LDX (Absolute Base) 4
        0xB6 -> LDX (ZeroPage Y) 4
        0xBE -> LDX (Absolute Y) 4
        0xA0 -> LDY Immediate 2
        0xA4 -> LDY (ZeroPage Base) 3
        0xAC -> LDY (Absolute Base) 4
        0xB4 -> LDY (ZeroPage X) 4
        0xBC -> LDY (Absolute X) 4
        0x81 -> STA (Indirect X) 6
        0x85 -> STA (ZeroPage Base) 3
        0x8D -> STA (Absolute Base) 4
        0x91 -> STA (Indirect Y) 6
        0x95 -> STA (ZeroPage X) 4
        0x99 -> STA (Absolute Y) 5
        0x9D -> STA (Absolute X) 5
        0x86 -> STX (ZeroPage Base) 3
        0x8E -> STX (Absolute Base) 4
        0x96 -> STX (ZeroPage Y) 4
        0x84 -> STY (ZeroPage Base) 3
        0x8C -> STY (Absolute Base) 4
        0x94 -> STY (ZeroPage X) 4
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
        0x4C -> JMP (Absolute Base) 3
        0x6C -> JMP (Indirect Base) 5
        0x20 -> JSR (Absolute Base) 6
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
        0x24 -> BIT (ZeroPage Base) 3
        0x2C -> BIT (Absolute Base) 4
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
