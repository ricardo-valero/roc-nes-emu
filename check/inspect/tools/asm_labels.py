#!/usr/bin/env python3
"""Map RAM labels from the captainsouthbird/smb3 disassembly to addresses.

Usage: asm_labels.py <smb3.asm> [label ...]   (no labels = dump all)
Counts BOTH labeled and anonymous `.ds` lines - anonymous gaps shift
everything after them (the hard-won lesson of the SMB3 sprite hunt).
"""
import re, sys

lines = open(sys.argv[1]).read().split('\n')
wanted = set(sys.argv[2:])
addr = None
for ln in lines:
    m = re.match(r'\s*\.org\s+\$([0-9A-Fa-f]+)', ln)
    if m:
        addr = int(m.group(1), 16)
        continue
    m = re.match(r'\s*(?:(\w+):)?\s*\.ds\s+(\d+)', ln)
    if m and addr is not None:
        label, size = m.group(1), int(m.group(2))
        if label and (not wanted or label in wanted):
            print(f'{label} = ${addr:04X} ({size})')
        addr += size
