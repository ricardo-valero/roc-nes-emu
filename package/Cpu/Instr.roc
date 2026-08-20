# 6502 (2A03) structured decode table: one entry per opcode 0x00-0xFF.
#
# Where the older `Instruction.roc` carries a flat `Op` enum in a four-column
# record, this states each instruction's capability in the type: the payload
# admits only the addressing modes that family has, families with a single mode
# carry none at all, and the fused unofficial opcodes name the two operations
# they are built from. Cycle counts and the page-cross penalty are derived from
# access class and mode rather than transcribed per opcode.
#
# References: https://www.nesdev.org/wiki/CPU_unofficial_opcodes
#             https://www.oxyron.de/html/opcodes02.html
import /Cpu/Instruction

# The full addressing-mode set, and the per-family subsets that narrow it.
#
# Subsets are per *family*, not per register: `Load` admits every mode any load
# reaches, so `Load(X(ZeroPageX))` compiles even though LDX has no zero page,X
# form. Per-register precision would need a subset per register - 18 in total -
# and Roc closed tag unions do not widen (roc-lang/roc#10413 makes it a compiler
# invariant; declaring an open union is rejected outright, roc-lang/roc#9939), so
# each subset costs an explicit `widen_*` cast below. Six subsets cost 58 lines;
# eighteen cost 140, to catch mistakes the 256-row equivalence check at the
# bottom of this file already catches on every build.
#
# What stays unrepresentable: a branch with an absolute mode, a store with an
# immediate, a shift with an indirect - every mode/family mismatch.
Mode : [
    Implied,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndexedIndirect,
    IndirectIndexed,
    Relative,
]

Access : [Read, Write, ReadModifyWrite, NoOperand]

ReadMode : [Immediate, ZeroPage, ZeroPageX, ZeroPageY, Absolute, AbsoluteX, AbsoluteY, IndexedIndirect, IndirectIndexed]

WriteMode : [ZeroPage, ZeroPageX, ZeroPageY, Absolute, AbsoluteX, AbsoluteY, IndexedIndirect, IndirectIndexed]

SkipMode : [Immediate, ZeroPage, ZeroPageX, Absolute, AbsoluteX]

ShiftMode : [Accumulator, ZeroPage, ZeroPageX, Absolute, AbsoluteX]

IncDecMode : [ZeroPage, ZeroPageX, Absolute, AbsoluteX]

JumpMode : [Absolute, Indirect]

Instr := [
    # Other
    Break, # Brk
    Halt, # Kil (unofficial)
    Nop, # Nop (implied) - 0xEA official, the six implied twins unofficial
    Skip(SkipMode), # Dop/Top a.k.a. Skb/Skw (unofficial) - reads an operand and discards it
    # Increment / decrement
    Inc([Memory(IncDecMode), X, Y]), # Inc, Inx, Iny
    Dec([Memory(IncDecMode), X, Y]), # Dec, Dex, Dey
    # Arithmetic / logical
    Alu([Adc, Sbc, And, Ora, Eor, Cmp([A, X, Y]), Bit], ReadMode), 
    # Shift / rotate
    Shift([LeftArithmetic, RightLogical], ShiftMode), # Asl, Lsr
    Rotate([Left, Right], ShiftMode), # Rol, Ror
    # Load / store
    Load([
        A, # Lda
        X, # Ldx
        Y, # Ldy
        AandX, # Lax (unofficial)
       ], ReadMode),
    Store([
        A, # Sta
        X, # Stx
        Y, # Sty
        AandX, # Sax (unofficial)
    ], WriteMode),
    # Transfer / stack
    Transfer([AtoX, AtoY, StoX, XtoA, XtoS, YtoA]), # Tax, Tay, Tsx, Txa, Txs, Tya
    Push([A, Status]), # Pha, Php
    Pull([A, Status]), # Pla, Plp
    # Branch / flags
    Branch([Carry, Zero, Negative, Overflow], Bool), # Bcc, Bcs, Beq, Bne, Bmi, Bpl, Bvc, Bvs
    Status([
        Clear([Carry, Decimal, InterruptDisable, Overflow]), # Clc, Cld, Cli, Clv
        Set([Carry, Decimal, InterruptDisable]), # Sec, Sed, Sei
    ]),
    # Jumps / returns
    Jump(JumpMode), # Jmp
    JumpSubroutine, # Jsr
    ReturnFrom([Interrupt, Subroutine]), # Rti, Rts
    # Fused read-modify-write + alu (all unofficial)
    Fused(
        [Shift([LeftArithmetic, RightLogical]), Rotate([Left, Right]), Inc, Dec],
        [Or, And, Xor, Adc, Sbc, Cmp],
        WriteMode,
    ),
    # Slo = Fused(Shift(LeftArithmetic), Or,  m)   Rla = Fused(Rotate(Left),  And, m)
    # Sre = Fused(Shift(RightLogical),   Xor, m)   Rra = Fused(Rotate(Right), Adc, m)
    # Dcp = Fused(Dec,                   Cmp, m)   Isc = Fused(Inc,           Sbc, m)
    # Unstable / one-off unofficial opcodes: the mnemonic is kept because these
    # do not decompose into a verb phrase; the behaviour is in the comment.
    # Each is single-mode on hardware; the family subset is wider than that.
    Alr(ReadMode), # Alr (unofficial) - Immediate - A & imm, then LSR
    Anc(ReadMode), # Anc (unofficial) - Immediate - A & imm, N copied to C
    Arr(ReadMode), # Arr (unofficial) - Immediate - A & imm, then ROR, bespoke V/C
    Axs(ReadMode), # Axs (unofficial) - Immediate - (A & X) - imm -> X
    Las(ReadMode), # Las (unofficial) - AbsoluteY - mem & S -> A, X, S
    Lxa(ReadMode), # Lxa (unofficial) - Immediate - (A | 0xEE) & imm -> A, X
    Xaa(ReadMode), # Xaa (unofficial) - Immediate - (A | 0xEE) & X & imm -> A
    Tas(WriteMode), # Tas (unofficial) - AbsoluteY - A & X -> S; S & (high+1) -> mem
    Ahx(WriteMode), # Ahx (unofficial) - AbsoluteY/(zp),Y - A & X & (high+1) -> mem
    Shx(WriteMode), # Shx (unofficial) - AbsoluteY - X & (high+1) -> mem
    Shy(WriteMode), # Shy (unofficial) - AbsoluteX - Y & (high+1) -> mem
].{
    lookup : U8 -> Instr
    lookup = |byte|
        match byte {
            0x00 => Break                                               # BRK
            0x01 => Alu(Ora,IndexedIndirect)                               # ORA
            0x02 => Halt                                                # KIL (unofficial)
            0x03 => Fused(Shift(LeftArithmetic), Or, IndexedIndirect)   # SLO (unofficial)
            0x04 => Skip(ZeroPage)                                      # NOP (unofficial)
            0x05 => Alu(Ora, ZeroPage)                                       # ORA
            0x06 => Shift(LeftArithmetic, ZeroPage)                     # ASL
            0x07 => Fused(Shift(LeftArithmetic), Or, ZeroPage)          # SLO (unofficial)
            0x08 => Push(Status)                                        # PHP
            0x09 => Alu(Ora, Immediate)                                      # ORA
            0x0A => Shift(LeftArithmetic, Accumulator)                  # ASL
            0x0B => Anc(Immediate)                                      # ANC (unofficial)
            0x0C => Skip(Absolute)                                      # NOP (unofficial)
            0x0D => Alu(Ora, Absolute)                                       # ORA
            0x0E => Shift(LeftArithmetic, Absolute)                     # ASL
            0x0F => Fused(Shift(LeftArithmetic), Or, Absolute)          # SLO (unofficial)
            0x10 => Branch(Negative, Bool.False)                        # BPL
            0x11 => Alu(Ora, IndirectIndexed)                                # ORA
            0x12 => Halt                                                # KIL (unofficial)
            0x13 => Fused(Shift(LeftArithmetic), Or, IndirectIndexed)   # SLO (unofficial)
            0x14 => Skip(ZeroPageX)                                     # NOP (unofficial)
            0x15 => Alu(Ora, ZeroPageX)                                      # ORA
            0x16 => Shift(LeftArithmetic, ZeroPageX)                    # ASL
            0x17 => Fused(Shift(LeftArithmetic), Or, ZeroPageX)         # SLO (unofficial)
            0x18 => Status(Clear(Carry))                                # CLC
            0x19 => Alu(Ora, AbsoluteY)                                      # ORA
            0x1A => Nop                                                 # NOP (unofficial)
            0x1B => Fused(Shift(LeftArithmetic), Or, AbsoluteY)         # SLO (unofficial)
            0x1C => Skip(AbsoluteX)                                     # NOP (unofficial)
            0x1D => Alu(Ora, AbsoluteX)                                      # ORA
            0x1E => Shift(LeftArithmetic, AbsoluteX)                    # ASL
            0x1F => Fused(Shift(LeftArithmetic), Or, AbsoluteX)         # SLO (unofficial)
            0x20 => JumpSubroutine                                      # JSR
            0x21 => Alu(And, IndexedIndirect)                                # AND
            0x22 => Halt                                                # KIL (unofficial)
            0x23 => Fused(Rotate(Left), And, IndexedIndirect)           # RLA (unofficial)
            0x24 => Alu(Bit, ZeroPage)                                       # BIT
            0x25 => Alu(And, ZeroPage)                                       # AND
            0x26 => Rotate(Left, ZeroPage)                              # ROL
            0x27 => Fused(Rotate(Left), And, ZeroPage)                  # RLA (unofficial)
            0x28 => Pull(Status)                                        # PLP
            0x29 => Alu(And,Immediate)                                      # AND
            0x2A => Rotate(Left, Accumulator)                           # ROL
            0x2B => Anc(Immediate)                                      # ANC (unofficial)
            0x2C => Alu(Bit, Absolute)                                       # BIT
            0x2D => Alu(And, Absolute)                                       # AND
            0x2E => Rotate(Left, Absolute)                              # ROL
            0x2F => Fused(Rotate(Left), And, Absolute)                  # RLA (unofficial)
            0x30 => Branch(Negative, Bool.True)                         # BMI
            0x31 => Alu(And,IndirectIndexed)                                # AND
            0x32 => Halt                                                # KIL (unofficial)
            0x33 => Fused(Rotate(Left), And, IndirectIndexed)           # RLA (unofficial)
            0x34 => Skip(ZeroPageX)                                     # NOP (unofficial)
            0x35 => Alu(And, ZeroPageX)                                      # AND
            0x36 => Rotate(Left, ZeroPageX)                             # ROL
            0x37 => Fused(Rotate(Left), And, ZeroPageX)                 # RLA (unofficial)
            0x38 => Status(Set(Carry))                                  # SEC
            0x39 => Alu(And, AbsoluteY)                                      # AND
            0x3A => Nop                                                 # NOP (unofficial)
            0x3B => Fused(Rotate(Left), And, AbsoluteY)                 # RLA (unofficial)
            0x3C => Skip(AbsoluteX)                                     # NOP (unofficial)
            0x3D => Alu(And, AbsoluteX)                                      # AND
            0x3E => Rotate(Left, AbsoluteX)                             # ROL
            0x3F => Fused(Rotate(Left), And, AbsoluteX)                 # RLA (unofficial)
            0x40 => ReturnFrom(Interrupt)                               # RTI
            0x41 => Alu(Eor, IndexedIndirect)                                # EOR
            0x42 => Halt                                                # KIL (unofficial)
            0x43 => Fused(Shift(RightLogical), Xor, IndexedIndirect)    # SRE (unofficial)
            0x44 => Skip(ZeroPage)                                      # NOP (unofficial)
            0x45 => Alu(Eor, ZeroPage)                                       # EOR
            0x46 => Shift(RightLogical, ZeroPage)                       # LSR
            0x47 => Fused(Shift(RightLogical), Xor, ZeroPage)           # SRE (unofficial)
            0x48 => Push(A)                                             # PHA
            0x49 => Alu(Eor,Immediate)                                      # EOR
            0x4A => Shift(RightLogical, Accumulator)                    # LSR
            0x4B => Alr(Immediate)                                      # ALR (unofficial)
            0x4C => Jump(Absolute)                                      # JMP
            0x4D => Alu(Eor, Absolute)                                       # EOR
            0x4E => Shift(RightLogical, Absolute)                       # LSR
            0x4F => Fused(Shift(RightLogical), Xor, Absolute)           # SRE (unofficial)
            0x50 => Branch(Overflow, Bool.False)                        # BVC
            0x51 => Alu(Eor, IndirectIndexed)                                # EOR
            0x52 => Halt                                                # KIL (unofficial)
            0x53 => Fused(Shift(RightLogical), Xor, IndirectIndexed)    # SRE (unofficial)
            0x54 => Skip(ZeroPageX)                                     # NOP (unofficial)
            0x55 => Alu(Eor, ZeroPageX)                                      # EOR
            0x56 => Shift(RightLogical, ZeroPageX)                      # LSR
            0x57 => Fused(Shift(RightLogical), Xor, ZeroPageX)          # SRE (unofficial)
            0x58 => Status(Clear(InterruptDisable))                     # CLI
            0x59 => Alu(Eor, AbsoluteY)                                      # EOR
            0x5A => Nop                                                 # NOP (unofficial)
            0x5B => Fused(Shift(RightLogical), Xor, AbsoluteY)          # SRE (unofficial)
            0x5C => Skip(AbsoluteX)                                     # NOP (unofficial)
            0x5D => Alu(Eor, AbsoluteX)                                      # EOR
            0x5E => Shift(RightLogical, AbsoluteX)                      # LSR
            0x5F => Fused(Shift(RightLogical), Xor, AbsoluteX)          # SRE (unofficial)
            0x60 => ReturnFrom(Subroutine)                              # RTS
            0x61 => Alu(Adc, IndexedIndirect)                                # ADC
            0x62 => Halt                                                # KIL (unofficial)
            0x63 => Fused(Rotate(Right), Adc, IndexedIndirect)          # RRA (unofficial)
            0x64 => Skip(ZeroPage)                                      # NOP (unofficial)
            0x65 => Alu(Adc, ZeroPage)                                       # ADC
            0x66 => Rotate(Right, ZeroPage)                             # ROR
            0x67 => Fused(Rotate(Right), Adc, ZeroPage)                 # RRA (unofficial)
            0x68 => Pull(A)                                             # PLA
            0x69 => Alu(Adc, Immediate)                                      # ADC
            0x6A => Rotate(Right, Accumulator)                          # ROR
            0x6B => Arr(Immediate)                                      # ARR (unofficial)
            0x6C => Jump(Indirect)                                      # JMP
            0x6D => Alu(Adc, Absolute)                                       # ADC
            0x6E => Rotate(Right, Absolute)                             # ROR
            0x6F => Fused(Rotate(Right), Adc, Absolute)                 # RRA (unofficial)
            0x70 => Branch(Overflow, Bool.True)                         # BVS
            0x71 => Alu(Adc, IndirectIndexed)                                # ADC
            0x72 => Halt                                                # KIL (unofficial)
            0x73 => Fused(Rotate(Right), Adc, IndirectIndexed)          # RRA (unofficial)
            0x74 => Skip(ZeroPageX)                                     # NOP (unofficial)
            0x75 => Alu(Adc, ZeroPageX)                                      # ADC
            0x76 => Rotate(Right, ZeroPageX)                            # ROR
            0x77 => Fused(Rotate(Right), Adc, ZeroPageX)                # RRA (unofficial)
            0x78 => Status(Set(InterruptDisable))                       # SEI
            0x79 => Alu(Adc,    AbsoluteY)                                      # ADC
            0x7A => Nop                                                 # NOP (unofficial)
            0x7B => Fused(Rotate(Right), Adc, AbsoluteY)                # RRA (unofficial)
            0x7C => Skip(AbsoluteX)                                     # NOP (unofficial)
            0x7D => Alu(Adc, AbsoluteX)                                      # ADC
            0x7E => Rotate(Right, AbsoluteX)                            # ROR
            0x7F => Fused(Rotate(Right), Adc, AbsoluteX)                # RRA (unofficial)
            0x80 => Skip(Immediate)                                     # NOP (unofficial)
            0x81 => Store(A,IndexedIndirect)                           # STA
            0x82 => Skip(Immediate)                                     # NOP (unofficial)
            0x83 => Store(AandX,IndexedIndirect)                       # SAX (unofficial)
            0x84 => Store(Y,ZeroPage)                                  # STY
            0x85 => Store(A,ZeroPage)                                  # STA
            0x86 => Store(X,ZeroPage)                                  # STX
            0x87 => Store(AandX,ZeroPage)                              # SAX (unofficial)
            0x88 => Dec(Y)                                              # DEY
            0x89 => Skip(Immediate)                                     # NOP (unofficial)
            0x8A => Transfer(XtoA)                                      # TXA
            0x8B => Xaa(Immediate)                                      # XAA (unofficial)
            0x8C => Store(Y,Absolute)                                  # STY
            0x8D => Store(A,Absolute)                                  # STA
            0x8E => Store(X,Absolute)                                  # STX
            0x8F => Store(AandX,Absolute)                              # SAX (unofficial)
            0x90 => Branch(Carry, Bool.False)                           # BCC
            0x91 => Store(A,IndirectIndexed)                           # STA
            0x92 => Halt                                                # KIL (unofficial)
            0x93 => Ahx(IndirectIndexed)                                # AHX (unofficial)
            0x94 => Store(Y,ZeroPageX)                                 # STY
            0x95 => Store(A,ZeroPageX)                                 # STA
            0x96 => Store(X,ZeroPageY)                                 # STX
            0x97 => Store(AandX,ZeroPageY)                             # SAX (unofficial)
            0x98 => Transfer(YtoA)                                      # TYA
            0x99 => Store(A,AbsoluteY)                                 # STA
            0x9A => Transfer(XtoS)                                      # TXS
            0x9B => Tas(AbsoluteY)                                      # TAS (unofficial)
            0x9C => Shy(AbsoluteX)                                      # SHY (unofficial)
            0x9D => Store(A,AbsoluteX)                                 # STA
            0x9E => Shx(AbsoluteY)                                      # SHX (unofficial)
            0x9F => Ahx(AbsoluteY)                                      # AHX (unofficial)
            0xA0 => Load(Y,Immediate)                                  # LDY
            0xA1 => Load(A,IndexedIndirect)                            # LDA
            0xA2 => Load(X,Immediate)                                  # LDX
            0xA3 => Load(AandX,IndexedIndirect)                        # LAX (unofficial)
            0xA4 => Load(Y,ZeroPage)                                   # LDY
            0xA5 => Load(A,ZeroPage)                                   # LDA
            0xA6 => Load(X,ZeroPage)                                   # LDX
            0xA7 => Load(AandX,ZeroPage)                               # LAX (unofficial)
            0xA8 => Transfer(AtoY)                                      # TAY
            0xA9 => Load(A,Immediate)                                  # LDA
            0xAA => Transfer(AtoX)                                      # TAX
            0xAB => Lxa(Immediate)                                      # LXA (unofficial)
            0xAC => Load(Y,Absolute)                                   # LDY
            0xAD => Load(A,Absolute)                                   # LDA
            0xAE => Load(X,Absolute)                                   # LDX
            0xAF => Load(AandX,Absolute)                               # LAX (unofficial)
            0xB0 => Branch(Carry, Bool.True)                            # BCS
            0xB1 => Load(A,IndirectIndexed)                            # LDA
            0xB2 => Halt                                                # KIL (unofficial)
            0xB3 => Load(AandX,IndirectIndexed)                        # LAX (unofficial)
            0xB4 => Load(Y,ZeroPageX)                                  # LDY
            0xB5 => Load(A,ZeroPageX)                                  # LDA
            0xB6 => Load(X,ZeroPageY)                                  # LDX
            0xB7 => Load(AandX,ZeroPageY)                              # LAX (unofficial)
            0xB8 => Status(Clear(Overflow))                             # CLV
            0xB9 => Load(A,AbsoluteY)                                  # LDA
            0xBA => Transfer(StoX)                                      # TSX
            0xBB => Las(AbsoluteY)                                      # LAS (unofficial)
            0xBC => Load(Y,AbsoluteX)                                  # LDY
            0xBD => Load(A,AbsoluteX)                                  # LDA
            0xBE => Load(X,AbsoluteY)                                  # LDX
            0xBF => Load(AandX,AbsoluteY)                              # LAX (unofficial)
            0xC0 => Alu(Cmp(Y), (Immediate))                                   # CPY
            0xC1 => Alu(Cmp(A), (IndexedIndirect))                             # CMP
            0xC2 => Skip(Immediate)                                     # NOP (unofficial)
            0xC3 => Fused(Dec, Cmp, IndexedIndirect)                    # DCP (unofficial)
            0xC4 => Alu(Cmp(Y), (ZeroPage))                                    # CPY
            0xC5 => Alu(Cmp(A), (ZeroPage))                                    # CMP
            0xC6 => Dec(Memory(ZeroPage))                               # DEC
            0xC7 => Fused(Dec, Cmp, ZeroPage)                           # DCP (unofficial)
            0xC8 => Inc(Y)                                              # INY
            0xC9 => Alu(Cmp(A), (Immediate))                                   # CMP
            0xCA => Dec(X)                                              # DEX
            0xCB => Axs(Immediate)                                      # AXS (unofficial)
            0xCC => Alu(Cmp(Y), (Absolute))                                    # CPY
            0xCD => Alu(Cmp(A), (Absolute))                                    # CMP
            0xCE => Dec(Memory(Absolute))                               # DEC
            0xCF => Fused(Dec, Cmp, Absolute)                           # DCP (unofficial)
            0xD0 => Branch(Zero, Bool.False)                            # BNE
            0xD1 => Alu(Cmp(A), (IndirectIndexed))                             # CMP
            0xD2 => Halt                                                # KIL (unofficial)
            0xD3 => Fused(Dec, Cmp, IndirectIndexed)                    # DCP (unofficial)
            0xD4 => Skip(ZeroPageX)                                     # NOP (unofficial)
            0xD5 => Alu(Cmp(A), (ZeroPageX))                                   # CMP
            0xD6 => Dec(Memory(ZeroPageX))                              # DEC
            0xD7 => Fused(Dec, Cmp, ZeroPageX)                          # DCP (unofficial)
            0xD8 => Status(Clear(Decimal))                              # CLD
            0xD9 => Alu(Cmp(A), (AbsoluteY))                                   # CMP
            0xDA => Nop                                                 # NOP (unofficial)
            0xDB => Fused(Dec, Cmp, AbsoluteY)                          # DCP (unofficial)
            0xDC => Skip(AbsoluteX)                                     # NOP (unofficial)
            0xDD => Alu(Cmp(A), (AbsoluteX))                                   # CMP
            0xDE => Dec(Memory(AbsoluteX))                              # DEC
            0xDF => Fused(Dec, Cmp, AbsoluteX)                          # DCP (unofficial)
            0xE0 => Alu(Cmp(X), (Immediate))                                   # CPX
            0xE1 => Alu(Sbc, IndexedIndirect)                                # SBC
            0xE2 => Skip(Immediate)                                     # NOP (unofficial)
            0xE3 => Fused(Inc, Sbc, IndexedIndirect)                    # ISC (unofficial)
            0xE4 => Alu(Cmp(X), (ZeroPage))                                    # CPX
            0xE5 => Alu(Sbc, ZeroPage)                                       # SBC
            0xE6 => Inc(Memory(ZeroPage))                               # INC
            0xE7 => Fused(Inc, Sbc, ZeroPage)                           # ISC (unofficial)
            0xE8 => Inc(X)                                              # INX
            0xE9 => Alu(Sbc, Immediate)                                      # SBC
            0xEA => Nop                                                 # NOP
            0xEB => Alu(Sbc, Immediate)                                      # SBC (unofficial)
            0xEC => Alu(Cmp(X),(Absolute))                                    # CPX
            0xED => Alu(Sbc, Absolute)                                       # SBC
            0xEE => Inc(Memory(Absolute))                               # INC
            0xEF => Fused(Inc, Sbc, Absolute)                           # ISC (unofficial)
            0xF0 => Branch(Zero, Bool.True)                             # BEQ
            0xF1 => Alu(Sbc, IndirectIndexed)                                # SBC
            0xF2 => Halt                                                # KIL (unofficial)
            0xF3 => Fused(Inc, Sbc, IndirectIndexed)                    # ISC (unofficial)
            0xF4 => Skip(ZeroPageX)                                     # NOP (unofficial)
            0xF5 => Alu(Sbc, ZeroPageX)                                      # SBC
            0xF6 => Inc(Memory(ZeroPageX))                              # INC
            0xF7 => Fused(Inc, Sbc, ZeroPageX)                          # ISC (unofficial)
            0xF8 => Status(Set(Decimal))                                # SED
            0xF9 => Alu(Sbc, AbsoluteY)                                      # SBC
            0xFA => Nop                                                 # NOP (unofficial)
            0xFB => Fused(Inc, Sbc, AbsoluteY)                          # ISC (unofficial)
            0xFC => Skip(AbsoluteX)                                     # NOP (unofficial)
            0xFD => Alu(Sbc, AbsoluteX)                                      # SBC
            0xFE => Inc(Memory(AbsoluteX))                              # INC
            0xFF => Fused(Inc, Sbc, AbsoluteX)                          # ISC (unofficial)
            _ => Halt # unreachable: all 256 byte values are listed above
        }

    # --- derived projections -------------------------------------------------
    # The addressing mode an instruction resolves through. Families with a
    # single mode report it here rather than carrying it in the payload.
    # Closed tag unions do not widen in Roc, so each narrowed payload is mapped
    # back into `Mode` by the corresponding `widen_*` below.
    mode : Instr -> Mode
    mode = |instr|
        match instr {
            Break => Implied
            Halt => Implied
            Nop => Implied
            Skip(m) => widen_skip(m)
            Inc(Memory(m)) => widen_incdec(m)
            Inc(_) => Implied
            Dec(Memory(m)) => widen_incdec(m)
            Dec(_) => Implied
            Alu(_, m) => widen_read(m)
            Shift(_, m) => widen_shift(m)
            Rotate(_, m) => widen_shift(m)
            Load(_, m) => widen_read(m)
            Store(_, m) => widen_write(m)
            Transfer(_) => Implied
            Push(_) => Implied
            Pull(_) => Implied
            Branch(_, _) => Relative
            Status(_) => Implied
            Jump(m) => widen_jump(m)
            JumpSubroutine => Absolute
            ReturnFrom(_) => Implied
            Fused(_, _, m) => widen_write(m)
            Alr(m) => widen_read(m)
            Anc(m) => widen_read(m)
            Arr(m) => widen_read(m)
            Axs(m) => widen_read(m)
            Las(m) => widen_read(m)
            Lxa(m) => widen_read(m)
            Xaa(m) => widen_read(m)
            Tas(m) => widen_write(m)
            Ahx(m) => widen_write(m)
            Shx(m) => widen_write(m)
            Shy(m) => widen_write(m)
        }

    # How the instruction touches memory. The page-cross penalty and the base
    # cycle cost are both computed from this.
    access : Instr -> Access
    access = |instr|
        match instr {
            Skip(_) => Read
            Load(_, _) => Read
            Alu(_, _) => Read
            Alr(_) => Read
            Anc(_) => Read
            Arr(_) => Read
            Axs(_) => Read
            Las(_) => Read
            Lxa(_) => Read
            Xaa(_) => Read
            Store(_, _) => Write
            Ahx(_) => Write
            Shx(_) => Write
            Shy(_) => Write
            Tas(_) => Write
            Shift(_, _) => ReadModifyWrite
            Rotate(_, _) => ReadModifyWrite
            Fused(_, _, _) => ReadModifyWrite
            Inc(Memory(_)) => ReadModifyWrite
            Dec(Memory(_)) => ReadModifyWrite
            _ => NoOperand
        }

    # +1 cycle when an indexed read crosses a page boundary. Stores and
    # read-modify-write instructions pay the fixed cost instead, so the whole
    # rule is: read-class instruction in a mode that can cross.
    penalty : Instr -> Bool
    penalty = |instr|
        match access(instr) {
            Read =>
                match mode(instr) {
                    AbsoluteX => Bool.True
                    AbsoluteY => Bool.True
                    IndirectIndexed => Bool.True
                    _ => Bool.False
                }

            _ => Bool.False
        }

    # Base cycle count. Every instruction with an addressing mode follows the
    # access-class table; the exemptions are control and stack instructions,
    # each of which carries no addressing mode.
    base_cycles : Instr -> U8
    base_cycles = |instr|
        match instr {
            Break => 7
            Halt => 11 # sentinel: KIL jams the CPU rather than retiring
            JumpSubroutine => 6
            ReturnFrom(_) => 6
            Push(_) => 3
            Pull(_) => 4
            Jump(Absolute) => 3
            Jump(Indirect) => 5
            _ => class_cycles(access(instr), mode(instr))
        }

    class_cycles : Access, Mode -> U8
    class_cycles = |acc, m|
        match acc {
            Read =>
                match m {
                    Immediate => 2
                    ZeroPage => 3
                    ZeroPageX => 4
                    ZeroPageY => 4
                    Absolute => 4
                    AbsoluteX => 4
                    AbsoluteY => 4
                    IndexedIndirect => 6
                    IndirectIndexed => 5
                    _ => 2
                }

            Write =>
                match m {
                    ZeroPage => 3
                    ZeroPageX => 4
                    ZeroPageY => 4
                    Absolute => 4
                    AbsoluteX => 5
                    AbsoluteY => 5
                    IndexedIndirect => 6
                    IndirectIndexed => 6
                    _ => 2
                }

            ReadModifyWrite =>
                match m {
                    Accumulator => 2
                    ZeroPage => 5
                    ZeroPageX => 6
                    ZeroPageY => 6
                    Absolute => 6
                    AbsoluteX => 7
                    AbsoluteY => 7
                    IndexedIndirect => 8
                    IndirectIndexed => 8
                    _ => 2
                }

            _ => 2
        }

    # --- widening ------------------------------------------------------------
    # Roc closed tag unions do not widen, so each narrowed payload needs an
    # explicit cast back into `Mode`. One per family subset; see the note at the
    # top of this file for why there are six of these and not eighteen.

    widen_read : ReadMode -> Mode
    widen_read = |m|
        match m {
            Immediate => Immediate
            ZeroPage => ZeroPage
            ZeroPageX => ZeroPageX
            ZeroPageY => ZeroPageY
            Absolute => Absolute
            AbsoluteX => AbsoluteX
            AbsoluteY => AbsoluteY
            IndexedIndirect => IndexedIndirect
            IndirectIndexed => IndirectIndexed
        }

    widen_write : WriteMode -> Mode
    widen_write = |m|
        match m {
            ZeroPage => ZeroPage
            ZeroPageX => ZeroPageX
            ZeroPageY => ZeroPageY
            Absolute => Absolute
            AbsoluteX => AbsoluteX
            AbsoluteY => AbsoluteY
            IndexedIndirect => IndexedIndirect
            IndirectIndexed => IndirectIndexed
        }

    widen_skip : SkipMode -> Mode
    widen_skip = |m|
        match m {
            Immediate => Immediate
            ZeroPage => ZeroPage
            ZeroPageX => ZeroPageX
            Absolute => Absolute
            AbsoluteX => AbsoluteX
        }

    widen_shift : ShiftMode -> Mode
    widen_shift = |m|
        match m {
            Accumulator => Accumulator
            ZeroPage => ZeroPage
            ZeroPageX => ZeroPageX
            Absolute => Absolute
            AbsoluteX => AbsoluteX
        }

    widen_incdec : IncDecMode -> Mode
    widen_incdec = |m|
        match m {
            ZeroPage => ZeroPage
            ZeroPageX => ZeroPageX
            Absolute => Absolute
            AbsoluteX => AbsoluteX
        }

    widen_jump : JumpMode -> Mode
    widen_jump = |m|
        match m {
            Absolute => Absolute
            Indirect => Indirect
        }
}

# --- equivalence with the transcribed reference table -------------------------
# `Instruction.roc` is the line-by-line transcription of the published opcode
# reference. These checks prove the structured decoder agrees with it on every
# opcode, so the derived cycle counts and penalties are pinned to that source
# rather than to this file's own reasoning.
agrees_at : U8 -> Bool
agrees_at = |byte| {
    structured = Instr.lookup(byte)
    reference = Instruction.lookup(byte)
    Instr.mode(structured) == reference.mode
    and Instr.base_cycles(structured) == reference.cycles
    and Instr.penalty(structured) == reference.penalty
}

all_agree : U8, U8 -> Bool
all_agree = |byte, remaining|
    if remaining == 0 {
        agrees_at(byte)
    } else if agrees_at(byte) {
        all_agree(byte.plus_wrap(1), remaining.minus_wrap(1))
    } else {
        Bool.False
    }

expect all_agree(0x00, 255) # all 256 opcodes match the reference table

# Shape checks. `Instr` has no equality method, so these match instead of ==.
shaped : U8, (Instr -> Bool) -> Bool
shaped = |byte, pred| pred(Instr.lookup(byte))

expect shaped(0xA9, |i| match i { Load(A,Immediate) => Bool.True, _ => Bool.False }) # LDA #
expect shaped(0xA3, |i| match i { Load(AandX,IndexedIndirect) => Bool.True, _ => Bool.False }) # LAX (d,X)
expect shaped(0x07, |i| match i { Fused(Shift(LeftArithmetic), Or, ZeroPage) => Bool.True, _ => Bool.False }) # SLO
expect shaped(0x47, |i| match i { Fused(Shift(RightLogical), Xor, ZeroPage) => Bool.True, _ => Bool.False }) # SRE is EOR, not AND
expect shaped(0x67, |i| match i { Fused(Rotate(Right), Adc, ZeroPage) => Bool.True, _ => Bool.False }) # RRA is ADC, not AND
expect shaped(0xC7, |i| match i { Fused(Dec, Cmp, ZeroPage) => Bool.True, _ => Bool.False }) # DCP
expect shaped(0x90, |i| match i { Branch(Carry, Bool.False) => Bool.True, _ => Bool.False }) # BCC
expect shaped(0x18, |i| match i { Status(Clear(Carry)) => Bool.True, _ => Bool.False }) # CLC
expect shaped(0xE8, |i| match i { Inc(X) => Bool.True, _ => Bool.False }) # INX

expect Instr.base_cycles(Instr.lookup(0xBD)) == 4 # LDA abs,X
expect Instr.penalty(Instr.lookup(0xBD)) == Bool.True # reads can cross
expect Instr.penalty(Instr.lookup(0x9D)) == Bool.False # STA abs,X pays the fixed 5
expect Instr.base_cycles(Instr.lookup(0x9D)) == 5
