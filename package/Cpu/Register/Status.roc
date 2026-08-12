import /Bit exposing [Bit]
# https://www.oxyron.de/html/opcodes02.html

Status :: [].{
    ## Status register
    ##
    ## Carry (C): 1 on unsigned overflow
    ## Zero (Z): 1 when all bits of a result are 0
    ## Interrupt disable (I): when 1, no interupts will occur (exceptions are IRQs forced by BRK and NMIs) IRQs = interrupt requests
    ## Decimal mode (D): 1 when CPU in BCD mode
    ## Break (B): 1 when interrupt was caused by a BRK
    ## Unused: always 1
    ## Overflow (V): 1 on signed overflow
    ## Negative (N): 1 when result is negative
    ##
    ## 0b1010_1010
    ##   ││││ │││╰─ Carry
    ##   ││││ ││╰── Zero
    ##   ││││ │╰─── Interrupt disable
    ##   ││││ ╰──── Decimal mode
    ##   │││╰─ Break
    ##   ││╰── Unused
    ##   │╰─── Overflow
    ##   ╰──── Negative
    Member := [Carry, Zero, InterruptDisable, DecimalMode, Break, Overflow, Negative]

    to_bit : Member -> Bit
    to_bit = |member|
        match member {
            Carry => B0
            Zero => B1
            InterruptDisable => B2
            DecimalMode => B3
            Break => B4
            Overflow => B6
            Negative => B7
        }

    mask : Member -> U8
    mask = |member| Bit.mask(to_bit(member))

    check : Member -> (U8 -> Bool)
    check = |member| |byte| Bit.check(to_bit(member), byte)

    modify : Member, Bool -> (U8 -> U8)
    modify = |member, bit| |byte|
        if bit {
            Bit.set(to_bit(member), byte)
        } else {
            Bit.clear(to_bit(member), byte)
        }
}

expect Status.mask(Zero) == 0b0000_0010

expect {
    f = Status.check(Carry)
    f(0b0101_0001) == Bool.True
}

expect {
    f = Status.modify(Carry, Bool.False)
    f(0b0101_0001) == 0b0101_0000
}
