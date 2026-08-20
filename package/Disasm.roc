import /Cpu/Instr
import /Cartridge

# Pure 6502 disassembler over the emulator's own decode table and mapper.
# Every opcode (official + unofficial) renders through `Instr.lookup`,
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

	# Conventional 6502 spelling, reconstructed from the structured tag.
	# Runs the opposite direction to `Instr.lookup`: the tag says what the
	# instruction does, this says what assemblers call it.
	mnemonic = |instr|
		match instr {
			Break => "BRK"
			Halt => "KIL"
			Nop => "NOP"
			Skip(_) => "NOP"
			Inc(Memory(_)) => "INC"
			Inc(X) => "INX"
			Inc(_) => "INY"
			Dec(Memory(_)) => "DEC"
			Dec(X) => "DEX"
			Dec(_) => "DEY"
			Alu(Adc, _) => "ADC"
			Alu(Sbc, _) => "SBC"
			Alu(And, _) => "AND"
			Alu(Ora, _) => "ORA"
			Alu(Eor, _) => "EOR"
			Alu(Cmp(A), _) => "CMP"
			Alu(Cmp(X), _) => "CPX"
			Alu(Cmp(Y), _) => "CPY"
			Alu(Bit, _) => "BIT"
			Shift(LeftArithmetic, _) => "ASL"
			Shift(RightLogical, _) => "LSR"
			Rotate(Left, _) => "ROL"
			Rotate(Right, _) => "ROR"
			Load(A, _) => "LDA"
			Load(X, _) => "LDX"
			Load(Y, _) => "LDY"
			Load(AandX, _) => "LAX"
			Store(A, _) => "STA"
			Store(X, _) => "STX"
			Store(Y, _) => "STY"
			Store(AandX, _) => "SAX"
			Transfer(AtoX) => "TAX"
			Transfer(AtoY) => "TAY"
			Transfer(StoX) => "TSX"
			Transfer(XtoA) => "TXA"
			Transfer(XtoS) => "TXS"
			Transfer(YtoA) => "TYA"
			Push(A) => "PHA"
			Push(Status) => "PHP"
			Pull(A) => "PLA"
			Pull(Status) => "PLP"
			Branch(Carry, Bool.False) => "BCC"
			Branch(Carry, Bool.True) => "BCS"
			Branch(Zero, Bool.False) => "BNE"
			Branch(Zero, Bool.True) => "BEQ"
			Branch(Negative, Bool.False) => "BPL"
			Branch(Negative, Bool.True) => "BMI"
			Branch(Overflow, Bool.False) => "BVC"
			Branch(Overflow, _) => "BVS"
			Status(Clear(Carry)) => "CLC"
			Status(Clear(Decimal)) => "CLD"
			Status(Clear(InterruptDisable)) => "CLI"
			Status(Clear(Overflow)) => "CLV"
			Status(Set(Carry)) => "SEC"
			Status(Set(Decimal)) => "SED"
			Status(Set(InterruptDisable)) => "SEI"
			Jump(_) => "JMP"
			JumpSubroutine => "JSR"
			ReturnFrom(Interrupt) => "RTI"
			ReturnFrom(Subroutine) => "RTS"
			# The fused pairs, spelled by which two operations they combine
			Fused(Shift(LeftArithmetic), Or, _) => "SLO"
			Fused(Shift(RightLogical), Xor, _) => "SRE"
			Fused(Rotate(Left), And, _) => "RLA"
			Fused(Rotate(Right), Adc, _) => "RRA"
			Fused(Dec, Cmp, _) => "DCP"
			Fused(Inc, Sbc, _) => "ISC"
			Fused(_, _, _) => "???" # unreachable: lookup emits only the six above
			Alr(_) => "ALR"
			Anc(_) => "ANC"
			Arr(_) => "ARR"
			Axs(_) => "AXS"
			Las(_) => "LAS"
			Lxa(_) => "LXA"
			Xaa(_) => "XAA"
			Tas(_) => "TAS"
			Ahx(_) => "AHX"
			Shx(_) => "SHX"
			Shy(_) => "SHY"
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
		inst = Instr.lookup(cart.read_prg(pc))
		b1 = cart.read_prg(pc.plus_wrap(1))
		lo = cart.read_prg(pc.plus_wrap(1)).to_u16()
		hi = cart.read_prg(pc.plus_wrap(2)).to_u16()
		abs = hi.shl_wrap(8).bitwise_or(lo)
		operand =
			match Instr.mode(inst) {
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
					disp = if b1 >= 128 {
						b1.to_u16().plus_wrap(0xFF00)
					} else {
						b1.to_u16()
					}
					" $${hex4(pc.plus_wrap(2).plus_wrap(disp))}"
				}
			}
		{ text: "${hex4(pc)}: ${mnemonic(inst)}${operand}", size: size_of(Instr.mode(inst)) }
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

# --- every mnemonic is unchanged from the pre-structured decoder ---------------
# The disassembler now reconstructs spellings from the structured tag rather than
# reading a flat `Op` enum. This pins all 256 of them; the expected string was
# taken from the flat table it replaced, so it is a cross-check, not a restatement.
all_mnemonics : U8, U8, Str -> Str
all_mnemonics = |byte, remaining, acc| {
	m = Disasm.mnemonic(Instr.lookup(byte))
	next = if acc == "" {
		m
	} else {
		"${acc} ${m}"
	}
	if remaining == 0 {
		next
	} else {
		all_mnemonics(byte.plus_wrap(1), remaining.minus_wrap(1), next)
	}
}

expect
	all_mnemonics(0x00, 255, "")
		== Str.join_with(
			["BRK ORA KIL SLO NOP ORA ASL SLO PHP ORA ASL ANC NOP ORA ASL SLO ", "BPL ORA KIL SLO NOP ORA ASL SLO CLC ORA NOP SLO NOP ORA ASL SLO ", "JSR AND KIL RLA BIT AND ROL RLA PLP AND ROL ANC BIT AND ROL RLA ", "BMI AND KIL RLA NOP AND ROL RLA SEC AND NOP RLA NOP AND ROL RLA ", "RTI EOR KIL SRE NOP EOR LSR SRE PHA EOR LSR ALR JMP EOR LSR SRE ", "BVC EOR KIL SRE NOP EOR LSR SRE CLI EOR NOP SRE NOP EOR LSR SRE ", "RTS ADC KIL RRA NOP ADC ROR RRA PLA ADC ROR ARR JMP ADC ROR RRA ", "BVS ADC KIL RRA NOP ADC ROR RRA SEI ADC NOP RRA NOP ADC ROR RRA ", "NOP STA NOP SAX STY STA STX SAX DEY NOP TXA XAA STY STA STX SAX ", "BCC STA KIL AHX STY STA STX SAX TYA STA TXS TAS SHY STA SHX AHX ", "LDY LDA LDX LAX LDY LDA LDX LAX TAY LDA TAX LXA LDY LDA LDX LAX ", "BCS LDA KIL LAX LDY LDA LDX LAX CLV LDA TSX LAS LDY LDA LDX LAX ", "CPY CMP NOP DCP CPY CMP DEC DCP INY CMP DEX AXS CPY CMP DEC DCP ", "BNE CMP KIL DCP NOP CMP DEC DCP CLD CMP NOP DCP NOP CMP DEC DCP ", "CPX SBC NOP ISC CPX SBC INC ISC INX SBC NOP SBC CPX SBC INC ISC ", "BEQ SBC KIL ISC NOP SBC INC ISC SED SBC NOP ISC NOP SBC INC ISC"],
			"",
		)
