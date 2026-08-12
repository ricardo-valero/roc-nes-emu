Bit := [B0, B1, B2, B3, B4, B5, B6, B7].{
    mask : Bit -> U8
    mask = |bit|
        U8.shl_wrap(
            1,
            match bit {
                B0 => 0 # 0x01
                B1 => 1 # 0x02
                B2 => 2 # 0x04
                B3 => 3 # 0x08
                B4 => 4 # 0x10
                B5 => 5 # 0x20
                B6 => 6 # 0x40
                B7 => 7 # 0x80
            },
        )

    # Set bit on byte
    set : Bit, U8 -> U8
    set = |bit, byte|
        byte.bitwise_or(bit.mask())

    # Reset bit on byte
    clear : Bit, U8 -> U8
    clear = |bit, byte|
        byte.bitwise_and(bit.mask().bitwise_not())

    # Toggle bit on byte
    toggle : Bit, U8 -> U8
    toggle = |bit, byte|
        byte.bitwise_xor(bit.mask())

    # Check bit on byte
    check : Bit, U8 -> Bool
    check = |bit, byte|
        byte.bitwise_and(bit.mask()) > 0
}

expect Bit.mask(B0) == 0b0000_0001
expect Bit.mask(B7) == 0b1000_0000

expect Bit.set(B1, 0b0000_0101) == 0b0000_0111
expect Bit.set(B6, 0b1110_0000) == 0b1110_0000

expect Bit.clear(B2, 0b000_01110) == 0b0000_1010
expect Bit.clear(B5, 0b010_10000) == 0b0101_0000

expect Bit.toggle(B2, 0b0000_0101) == 0b0000_0001
expect Bit.toggle(B2, 0b0000_0001) == 0b0000_0101

expect Bit.check(B3, 0b1110_1000) == Bool.True
expect Bit.check(B4, 0b1110_1000) == Bool.False
