app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    http: "https://github.com/roc-lang/http/releases/download/1.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
}

import pf.OsStr
import pf.Http
import pf.Path
import pf.Stdout
import http.Request

# Fetch blargg's $6000-status-protocol PPU test ROMs (nes-test-roms) into
# check/blargg-ppu/data/.
#
#   roc check/blargg-ppu/fetch.roc

base : Str
base = "https://raw.githubusercontent.com/christopherpow/nes-test-roms/master"

files : List({ name : Str, url_path : Str })
files = [
    { name: "01-vbl_basics.nes", url_path: "ppu_vbl_nmi/rom_singles/01-vbl_basics.nes" },
    { name: "02-vbl_set_time.nes", url_path: "ppu_vbl_nmi/rom_singles/02-vbl_set_time.nes" },
    { name: "03-vbl_clear_time.nes", url_path: "ppu_vbl_nmi/rom_singles/03-vbl_clear_time.nes" },
    { name: "04-nmi_control.nes", url_path: "ppu_vbl_nmi/rom_singles/04-nmi_control.nes" },
    { name: "05-nmi_timing.nes", url_path: "ppu_vbl_nmi/rom_singles/05-nmi_timing.nes" },
    { name: "06-suppression.nes", url_path: "ppu_vbl_nmi/rom_singles/06-suppression.nes" },
    { name: "07-nmi_on_timing.nes", url_path: "ppu_vbl_nmi/rom_singles/07-nmi_on_timing.nes" },
    { name: "08-nmi_off_timing.nes", url_path: "ppu_vbl_nmi/rom_singles/08-nmi_off_timing.nes" },
    { name: "09-even_odd_frames.nes", url_path: "ppu_vbl_nmi/rom_singles/09-even_odd_frames.nes" },
    { name: "10-even_odd_timing.nes", url_path: "ppu_vbl_nmi/rom_singles/10-even_odd_timing.nes" },
    { name: "oam_read.nes", url_path: "oam_read/oam_read.nes" },
    { name: "oam_stress.nes", url_path: "oam_stress/oam_stress.nes" },
    { name: "ppu_open_bus.nes", url_path: "ppu_open_bus/ppu_open_bus.nes" },
    { name: "mmc3-1-clocking.nes", url_path: "mmc3_test_2/rom_singles/1-clocking.nes" },
    { name: "mmc3-2-details.nes", url_path: "mmc3_test_2/rom_singles/2-details.nes" },
    { name: "mmc3-3-A12_clocking.nes", url_path: "mmc3_test_2/rom_singles/3-A12_clocking.nes" },
    { name: "mmc3-4-scanline_timing.nes", url_path: "mmc3_test_2/rom_singles/4-scanline_timing.nes" },
    { name: "mmc3-5-MMC3.nes", url_path: "mmc3_test_2/rom_singles/5-MMC3.nes" },
    { name: "mmc3-6-MMC3_alt.nes", url_path: "mmc3_test_2/rom_singles/6-MMC3_alt.nes" },
]

fetch_one! = |file| {
    path = Path.from_os_str(OsStr.from_str("check/blargg-ppu/data/${file.name}"))
    if path.is_file!() ?? Bool.False {
        Stdout.line!("${file.name}: already present")
    } else {
        response = Http.send!(Request.from_method(GET).with_uri("${base}/${file.url_path}"))?
        if response.status() == 200 {
            path.write_bytes!(response.body())?
            Stdout.line!("${file.name}: fetched (${response.body().len().to_str()} bytes)")
        } else {
            Stdout.line!("${file.name}: HTTP ${response.status().to_str()}")?
            Err(FetchFailed)
        }
    }
}

fetch_all! = |idx|
    match files.get(idx) {
        Err(_) => Ok({})
        Ok(file) => {
            fetch_one!(file)?
            fetch_all!(idx.plus(1))
        }
    }

main! : List(OsStr) => Try({}, _)
main! = |_args| {
    fetch_all!(0)?
    Stdout.line!("blargg PPU test data present in check/blargg-ppu/data/")
}
