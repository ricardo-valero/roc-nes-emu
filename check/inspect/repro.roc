app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    nes: "../../package/main.roc",
}

import pf.OsStr
import pf.Stdout
import nes.Bus
import nes.Cartridge
import nes.Ppu

# throwaway: bisect the compiled-vs-interpreted OAM divergence.
# Mirrors the Bus OAM-DMA expect (which passes under `roc test`).

main! : List(OsStr) => Try({}, _)
main! = |_args| {
    header = [0x4E, 0x45, 0x53, 0x1A, 1, 1, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0]
    rom = header.concat(List.repeat(0xEA, 16384)).concat(List.repeat(0, 8192))
    match Cartridge.from_bytes(rom) {
        Err(_) => Stdout.line!("bad rom")
        Ok(cart) => {
            # 1: plain Ppu.load_oam with a literal list
            p1 = Ppu.init(Vertical).load_oam(List.repeat(0x55, 256))
            Stdout.line!("load_oam direct: oam[0]=${(p1.oam.get(0) ?? 99).to_str()} (want 85)")?

            # 2: OAMDATA writes through the register path
            w = |pp, r, v| pp.write_reg(r, v).ppu
            p2 = w(w(w(Ppu.init(Vertical), 3, 0), 4, 0x66), 4, 0x67)
            Stdout.line!("oamdata writes: oam[0]=${(p2.oam.get(0) ?? 99).to_str()} oam[1]=${(p2.oam.get(1) ?? 99).to_str()} (want 102 103)")?

            # 3: full bus DMA (the failing path?)
            seeded = Bus.from_cartridge(cart).write8(0x0200, 0x11).write8(0x02FF, 0x99)
            dma = seeded.write8(0x4014, 0x02)
            r1 = dma.read8(0x2004)
            Stdout.line!("dma: oam[0]=${r1.value.to_str()} (want 17)")?

            # 4: the copy loop shape in isolation: recursive build of 256 bytes
            copy = |st, i| {
                if i > 255 {
                    st
                } else {
                    r = seeded.read8(i)
                    copy({ bus: r.bus, data: st.data.append(r.value) }, i.plus(1))
                }
            }
            start : { bus : Bus, data : List(U8) }
            start = { bus: seeded, data: [] }
            i0 : U16
            i0 = 0
            result = copy(start, i0)
            Stdout.line!("copy loop: len=${result.data.len().to_str()} (want 256)")?

            # 5: copy-loop CONTENT order: read8 over an ascending RAM region
            build_asc = |acc, k| if k > 255 { acc } else { build_asc(acc.append(k.to_u8_wrap()), k.plus(1)) }
            asc0 : List(U8)
            asc0 = []
            k0 : U16
            k0 = 0
            asc = build_asc(asc0, k0)
            ramped = asc.fold(seeded, |bb, v| bb.write8(v.to_u16(), v))
            copy2 = |st, i| {
                if i > 255 {
                    st
                } else {
                    r = st.bus.read8(i)
                    copy2({ bus: r.bus, data: st.data.append(r.value) }, i.plus(1))
                }
            }
            start2 : { bus : Bus, data : List(U8) }
            start2 = { bus: ramped, data: [] }
            r2 = copy2(start2, i0)
            Stdout.line!("copy order: d[0]=${(r2.data.get(0) ?? 99).to_str()} d[1]=${(r2.data.get(1) ?? 99).to_str()} d[255]=${(r2.data.get(255) ?? 99).to_str()} (want 0 1 255)")?

            # 6: load_oam with an ascending literal list: ordering check
            p3 = Ppu.init(Vertical).load_oam(asc)
            Stdout.line!("load_oam order: oam[0]=${(p3.oam.get(0) ?? 99).to_str()} oam[1]=${(p3.oam.get(1) ?? 99).to_str()} oam[255]=${(p3.oam.get(255) ?? 99).to_str()} (want 0 1 255)")
        }
    }
}
