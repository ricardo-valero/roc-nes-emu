Memory :: [].{
    Type : List(U8)

    read8 : Type, U16 -> U8
    read8 = |memory, addr|
        memory.get(addr.to_u64()) ?? 0

    write8 : Type, U16, U8 -> Type
    write8 = |memory, addr, data|
        memory.set(addr.to_u64(), data) ?? memory

    read16 : Type, U16 -> U16
    read16 = |memory, pos| {
        lo = read8(memory, pos)
        hi = read8(memory, pos.plus(1))
        hi.to_u16().shl_wrap(8).bitwise_or(lo.to_u16())
    }

    write16 : Type, U16, U16 -> Type
    write16 = |memory, pos, data| {
        hi = data.shr_zf_wrap(8).to_u8_wrap()
        lo = data.bitwise_and(0xFF).to_u8_wrap()
        write8(write8(memory, pos, lo), pos.plus(1), hi)
    }

    load : Type, List(U8) -> Type
    load = |mem, program| {
        helper = |current_mem, index| {
            if index >= program.len() {
                current_mem
            } else {
                updated_mem = write8(current_mem, index.to_u16_wrap().plus(0x8000), program.get(index) ?? 0)
                helper(updated_mem, index.plus(1))
            }
        }
        write16(helper(mem, 0), 0xFFFC, 0x8000)
    }
}
