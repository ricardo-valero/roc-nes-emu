module [check]

import Bit exposing [Bit]

## Status register
## 0b0101_1010
##   ││││ │││╰─ Carry
##   ││││ ││╰── Zero
##   ││││ │╰─── Disable interrupts
##   ││││ ╰──── Decimal mode
##   │││╰─ Break
##   ││╰── Unused
##   │╰─── Overflow
##   ╰──── Negative
Member : [Carry, Zero, DisableInterrupts, DecimalMode, Break, Overflow, Negative]

toBit : Member -> Bit
toBit = \member ->
    when member is
        Carry -> B0 # C
        Zero -> B1 # Z
        DisableInterrupts -> B2 # I
        DecimalMode -> B3 # D
        Break -> B4 # B
        Overflow -> B6 # V
        Negative -> B7 # N

mask : Member -> U8
mask = \member -> Bit.mask (toBit member)

expect mask Zero == 0b0000_0010

check : Member, U8 -> Bool
check = \member, byte -> Bit.check (toBit member) byte

expect check Carry 0b0101_0001 == Bool.true
