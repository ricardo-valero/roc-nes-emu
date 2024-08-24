module [Bit, mask, set, clear, toggle, check]

Bit : [B0, B1, B2, B3, B4, B5, B6, B7]

mask : Bit -> U8
mask = \bit ->
    Num.shiftLeftBy
        1
        (
            when bit is
                B0 -> 0 # 0x01
                B1 -> 1 # 0x02
                B2 -> 2 # 0x04
                B3 -> 3 # 0x08
                B4 -> 4 # 0x10
                B5 -> 5 # 0x20
                B6 -> 6 # 0x40
                B7 -> 7 # 0x80
        )

expect mask B0 == 0b0000_0001
expect mask B7 == 0b1000_0000

# Set bit on byte
set : Bit, U8 -> U8
set = \bit, byte ->
    Num.bitwiseOr byte (mask bit)

expect set B1 0b0000_0101 == 0b0000_0111
expect set B6 0b1110_0000 == 0b1110_0000

# Reset bit on byte
clear : Bit, U8 -> U8
clear = \bit, byte ->
    Num.bitwiseAnd byte (Num.bitwiseNot (mask bit))

expect clear B2 0b000_01110 == 0b0000_1010
expect clear B5 0b010_10000 == 0b0101_0000

# Toggle bit on byte
toggle : Bit, U8 -> U8
toggle = \bit, byte ->
    Num.bitwiseXor byte (mask bit)

expect toggle B2 0b0000_0101 == 0b0000_0001
expect toggle B2 0b0000_0001 == 0b0000_0101

# Check bit on byte
check : Bit, U8 -> Bool
check = \bit, byte ->
    Num.bitwiseAnd byte (mask bit) > 0

expect check B3 0b1110_1000 == Bool.true
expect check B4 0b1110_1000 == Bool.false
