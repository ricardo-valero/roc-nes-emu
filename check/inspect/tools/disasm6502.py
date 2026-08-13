#!/usr/bin/env python3
"""Minimal 6502 disassembler over an iNES ROM with explicit MMC3 banking.

Usage: disasm6502.py <rom.nes> <cpu_addr_hex> [--bank8000 N] [--bankA000 N] [--bankC000 N]
Fixed banks: 0xE000 = last 8K bank; unspecified windows default per MMC3
PRG mode 1 (0x8000 = second-to-last). Born in the SMB3 sprite hunt.
"""
import sys

OPS = {0xA9:('LDA','#'),0xA5:('LDA','zp'),0xAD:('LDA','abs'),0xBD:('LDA','absX'),0xB9:('LDA','absY'),0xB5:('LDA','zpX'),0xB1:('LDA','indY'),0xA1:('LDA','indX'),
0x85:('STA','zp'),0x8D:('STA','abs'),0x9D:('STA','absX'),0x99:('STA','absY'),0x91:('STA','indY'),0x95:('STA','zpX'),
0xA2:('LDX','#'),0xA6:('LDX','zp'),0xAE:('LDX','abs'),0xA0:('LDY','#'),0xA4:('LDY','zp'),0xAC:('LDY','abs'),
0x86:('STX','zp'),0x8E:('STX','abs'),0x84:('STY','zp'),0x8C:('STY','abs'),
0xC9:('CMP','#'),0xC5:('CMP','zp'),0xCD:('CMP','abs'),0xE0:('CPX','#'),0xC0:('CPY','#'),0xCC:('CPY','abs'),
0xD0:('BNE','rel'),0xF0:('BEQ','rel'),0x10:('BPL','rel'),0x30:('BMI','rel'),0x90:('BCC','rel'),0xB0:('BCS','rel'),0x50:('BVC','rel'),0x70:('BVS','rel'),
0x4C:('JMP','abs'),0x6C:('JMP','ind'),0x20:('JSR','abs'),0x60:('RTS','imp'),0x40:('RTI','imp'),
0xE6:('INC','zp'),0xC6:('DEC','zp'),0xEE:('INC','abs'),0xCE:('DEC','abs'),0xE8:('INX','imp'),0xC8:('INY','imp'),0xCA:('DEX','imp'),0x88:('DEY','imp'),
0x18:('CLC','imp'),0x38:('SEC','imp'),0x78:('SEI','imp'),0x58:('CLI','imp'),0xEA:('NOP','imp'),
0x29:('AND','#'),0x25:('AND','zp'),0x09:('ORA','#'),0x05:('ORA','zp'),0x49:('EOR','#'),0x45:('EOR','zp'),0x2C:('BIT','abs'),0x24:('BIT','zp'),
0x69:('ADC','#'),0x65:('ADC','zp'),0xE9:('SBC','#'),0xE5:('SBC','zp'),0x48:('PHA','imp'),0x68:('PLA','imp'),0x08:('PHP','imp'),0x28:('PLP','imp'),
0x8A:('TXA','imp'),0xAA:('TAX','imp'),0x98:('TYA','imp'),0xA8:('TAY','imp'),0x9A:('TXS','imp'),0xBA:('TSX','imp'),
0x4A:('LSR','imp'),0x0A:('ASL','imp'),0x2A:('ROL','imp'),0x6A:('ROR','imp'),0x46:('LSR','zp'),0x06:('ASL','zp'),0x66:('ROR','zp'),0x26:('ROL','zp')}
SIZES = {'#':2,'zp':2,'zpX':2,'abs':3,'absX':3,'absY':3,'rel':2,'imp':1,'ind':3,'indX':2,'indY':2}

def main():
    rom = open(sys.argv[1], 'rb').read()
    start = int(sys.argv[2], 16)
    prg_size = rom[4] * 16384
    prg = rom[16:16 + prg_size]
    banks8 = prg_size // 8192
    windows = {0x8000: banks8 - 2, 0xA000: 0, 0xC000: 0, 0xE000: banks8 - 1}
    args = sys.argv[3:]
    for i in range(0, len(args), 2):
        key = {'--bank8000': 0x8000, '--bankA000': 0xA000, '--bankC000': 0xC000}[args[i]]
        windows[key] = int(args[i + 1])

    def byte_at(addr):
        base = addr & 0xE000
        return prg[windows[base] * 0x2000 + (addr - base)]

    pc = start
    for _ in range(40):
        op = byte_at(pc)
        if op in OPS:
            name, mode = OPS[op]
            size = SIZES[mode]
            if size == 1:
                operand = ''
            elif size == 2:
                v = byte_at(pc + 1)
                if mode == 'rel':
                    target = pc + 2 + (v if v < 128 else v - 256)
                    operand = f'${target:04X}'
                else:
                    operand = f'#${v:02X}' if mode == '#' else f'${v:02X}'
            else:
                operand = f'${byte_at(pc+1) | (byte_at(pc+2)<<8):04X}'
            print(f'{pc:04X}: {name} {operand}')
            pc += size
        else:
            print(f'{pc:04X}: .db ${op:02X}')
            pc += 1

if __name__ == '__main__':
    main()
