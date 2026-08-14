app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    http: "https://github.com/roc-lang/http/releases/download/1.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
}

import pf.OsStr
import pf.Http
import pf.Path
import pf.Stdout
import http.Request

# Fetch blargg's $6000-status-protocol APU test ROMs (nes-test-roms) into
# check/blargg-apu/data/.
#
#   roc check/blargg-apu/fetch.roc

base : Str
base = "https://raw.githubusercontent.com/christopherpow/nes-test-roms/master"

files : List({ name : Str, url_path : Str })
files = [
    { name: "1-len_ctr.nes", url_path: "apu_test/rom_singles/1-len_ctr.nes" },
    { name: "2-len_table.nes", url_path: "apu_test/rom_singles/2-len_table.nes" },
    { name: "3-irq_flag.nes", url_path: "apu_test/rom_singles/3-irq_flag.nes" },
    { name: "4-jitter.nes", url_path: "apu_test/rom_singles/4-jitter.nes" },
    { name: "5-len_timing.nes", url_path: "apu_test/rom_singles/5-len_timing.nes" },
    { name: "6-irq_flag_timing.nes", url_path: "apu_test/rom_singles/6-irq_flag_timing.nes" },
    { name: "7-dmc_basics.nes", url_path: "apu_test/rom_singles/7-dmc_basics.nes" },
    { name: "8-dmc_rates.nes", url_path: "apu_test/rom_singles/8-dmc_rates.nes" },
]

fetch_one! = |file| {
    path = Path.from_os_str(OsStr.from_str("check/blargg-apu/data/${file.name}"))
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
    Stdout.line!("blargg APU test data present in check/blargg-apu/data/")
}
