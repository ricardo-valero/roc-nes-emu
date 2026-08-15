import /Cpu/Instruction
import /Cartridge

# Pure 6502 disassembler over the emulator's own decode table and mapper.
# Every opcode (official + unofficial) renders through `Instruction.lookup`,
# so the listing can never disagree with the CPU about meaning or size, and
# bytes are fetched through `Cartridge.read_prg`, so banking is the mapper's
# problem — the same PRG mapping the CPU sees. First building block of a
# future debugger (disassembler pane).
Disasm :: [].{
    # instruction size in bytes, determined by the addressing mode
    size_of = |mode|
        match mode {
            Implied => 1
            Accumulator => 1
            Immediate => 2
            ZeroPage => 2
            ZeroPageX => 2
            ZeroPageY => 2
            IndexedIndirect => 2
            IndirectIndexed => 2
            Relative => 2
            Absolute => 3
            AbsoluteX => 3
            AbsoluteY => 3
            Indirect => 3
        }

    mnemonic = |op|
        match op {
            # Official
            Adc => "ADC"
            And => "AND"
            Asl => "ASL"
            Bcc => "BCC"
            Bcs => "BCS"
            Beq => "BEQ"
            Bit => "BIT"
            Bmi => "BMI"
            Bne => "BNE"
            Bpl => "BPL"
            Brk => "BRK"
            Bvc => "BVC"
            Bvs => "BVS"
            Clc => "CLC"
            Cld => "CLD"
            Cli => "CLI"
            Clv => "CLV"
            Cmp => "CMP"
            Cpx => "CPX"
            Cpy => "CPY"
            Dec => "DEC"
            Dex => "DEX"
            Dey => "DEY"
            Eor => "EOR"
            Inc => "INC"
            Inx => "INX"
            Iny => "INY"
            Jmp => "JMP"
            Jsr => "JSR"
            Lda => "LDA"
            Ldx => "LDX"
            Ldy => "LDY"
            Lsr => "LSR"
            Nop => "NOP"
            Ora => "ORA"
            Pha => "PHA"
            Php => "PHP"
            Pla => "PLA"
            Plp => "PLP"
            Rol => "ROL"
            Ror => "ROR"
            Rti => "RTI"
            Rts => "RTS"
            Sbc => "SBC"
            Sec => "SEC"
            Sed => "SED"
            Sei => "SEI"
            Sta => "STA"
            Stx => "STX"
            Sty => "STY"
            Tax => "TAX"
            Tay => "TAY"
            Tsx => "TSX"
            Txa => "TXA"
            Txs => "TXS"
            Tya => "TYA"
            # Unofficial
            Ahx => "AHX"
            Alr => "ALR"
            Anc => "ANC"
            Arr => "ARR"
            Axs => "AXS"
            Dcp => "DCP"
            Isc => "ISC"
            Kil => "KIL"
            Las => "LAS"
            Lax => "LAX"
            Lxa => "LXA"
            Rla => "RLA"
            Rra => "RRA"
            Sax => "SAX"
            Shx => "SHX"
            Shy => "SHY"
            Slo => "SLO"
            Sre => "SRE"
            Tas => "TAS"
            Xaa => "XAA"
        }

    hex_digit : U8 -> Str
    hex_digit = |n|
        match n.bitwise_and(0x0F) {
            0 => "0"
            1 => "1"
            2 => "2"
            3 => "3"
            4 => "4"
            5 => "5"
            6 => "6"
            7 => "7"
            8 => "8"
            9 => "9"
            10 => "A"
            11 => "B"
            12 => "C"
            13 => "D"
            14 => "E"
            _ => "F"
        }

    hex2 : U8 -> Str
    hex2 = |b| "${hex_digit(b.shr_zf_wrap(4))}${hex_digit(b)}"

    hex4 : U16 -> Str
    hex4 = |w| "${hex2(w.shr_zf_wrap(8).to_u8_wrap())}${hex2(w.to_u8_wrap())}"

    # one rendered instruction at pc: `ADDR: MNEMONIC operand` + its size
    line : Cartridge, U16 -> { text : Str, size : U16 }
    line = |cart, pc| {
        inst = Instruction.lookup(cart.read_prg(pc))
        b1 = cart.read_prg(pc.plus_wrap(1))
        lo = cart.read_prg(pc.plus_wrap(1)).to_u16()
        hi = cart.read_prg(pc.plus_wrap(2)).to_u16()
        abs = hi.shl_wrap(8).bitwise_or(lo)
        operand =
            match inst.mode {
                Implied => ""
                Accumulator => " A"
                Immediate => " #$${hex2(b1)}"
                ZeroPage => " $${hex2(b1)}"
                ZeroPageX => " $${hex2(b1)},X"
                ZeroPageY => " $${hex2(b1)},Y"
                IndexedIndirect => " ($${hex2(b1)},X)"
                IndirectIndexed => " ($${hex2(b1)}),Y"
                Absolute => " $${hex4(abs)}"
                AbsoluteX => " $${hex4(abs)},X"
                AbsoluteY => " $${hex4(abs)},Y"
                Indirect => " ($${hex4(abs)})"
                Relative => {
                    # displacement is signed; target counts from the next instruction
                    disp = if b1 >= 128 { b1.to_u16().plus_wrap(0xFF00) } else { b1.to_u16() }
                    " $${hex4(pc.plus_wrap(2).plus_wrap(disp))}"
                }
            }
        { text: "${hex4(pc)}: ${mnemonic(inst.op)}${operand}", size: size_of(inst.mode) }
    }

    # `count` rendered lines starting at `start`, each advancing by the
    # decoded instruction's size
    listing : Cartridge, U16, U64 -> List(Str)
    listing = |cart, start, count| {
        go = |acc, pc, n|
            if n == 0 {
                acc
            } else {
                l = line(cart, pc)
                go(acc.append(l.text), pc.plus_wrap(l.size), n.minus(1))
            }
        z : U64
        z = 0
        go([], start, count.plus(z))
    }
}

# synthetic 16K NROM cartridge whose PRG starts with the given bytes
test_cart : List(U8) -> Try(Cartridge, [NotANesFile])
test_cart = |code| {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 0, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    prg = code.concat(List.repeat(0xEA, U64.minus(16384, code.len())))
    Cartridge.from_bytes(header.concat(prg))
}

# official opcode: LDA immediate, 2 bytes
expect {
    match test_cart([0xA9, 0x42]) {
        Ok(cart) => {
            l = Disasm.line(cart, 0x8000)
            l.text == "8000: LDA #$42" and l.size == 2
        }

        Err(_) => Bool.False
    }
}

# unofficial opcode: LAX zero-page renders with its own mnemonic and size 2,
# so the following instruction decodes at the right address
expect {
    match test_cart([0xA7, 0x10, 0xA9, 0x01]) {
        Ok(cart) => {
            l = Disasm.line(cart, 0x8000)
            l.text == "8000: LAX $10"
            and l.size == 2
            and Disasm.listing(cart, 0x8000, 2) == ["8000: LAX $10", "8002: LDA #$01"]
        }

        Err(_) => Bool.False
    }
}

# negative displacement: BNE -16 at 0x8100 targets 0x8102 - 16 = 0x80F2
expect {
    code = List.repeat(0xEA, 0x100).concat([0xD0, 0xF0])
    match test_cart(code) {
        Ok(cart) => Disasm.line(cart, 0x8100).text == "8100: BNE $80F2"
        Err(_) => Bool.False
    }
}

# 3-byte absolute with index suffix, and indirect jump
expect {
    match test_cart([0xBD, 0x00, 0x07, 0x6C, 0x34, 0x12]) {
        Ok(cart) =>
            Disasm.listing(cart, 0x8000, 2) == ["8000: LDA $0700,X", "8003: JMP ($1234)"]

        Err(_) => Bool.False
    }
}
