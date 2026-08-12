app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    http: "https://github.com/roc-lang/http/releases/download/1.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
}

import pf.OsStr
import pf.Http
import pf.Path
import pf.Stdout
import http.Request

# Fetch nestest.nes (kevtris' CPU test cartridge, from the nes-test-roms
# collection) and the canonical golden log into check/nestest/data/.
#
#   roc check/nestest/fetch.roc

files : List({ name : Str, url : Str })
files = [
    { name: "nestest.nes", url: "https://raw.githubusercontent.com/christopherpow/nes-test-roms/master/other/nestest.nes" },
    { name: "nestest.log", url: "https://www.qmtpro.com/~nes/misc/nestest.log" },
]

fetch_one! = |file| {
    path = Path.from_os_str(OsStr.from_str("check/nestest/data/${file.name}"))
    if path.is_file!() ?? Bool.False {
        Stdout.line!("${file.name}: already present")
    } else {
        response = Http.send!(Request.from_method(GET).with_uri(file.url))?
        if response.status() == 200 {
            path.write_bytes!(response.body())?
            Stdout.line!("${file.name}: fetched (${response.body().len().to_str()} bytes)")
        } else {
            Stdout.line!("${file.name}: HTTP ${response.status().to_str()} from ${file.url}")?
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
    Stdout.line!("nestest data present in check/nestest/data/")
}
