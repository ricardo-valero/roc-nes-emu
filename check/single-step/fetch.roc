app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    http: "https://github.com/roc-lang/http/releases/download/1.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
}

import pf.OsStr
import pf.Http
import pf.Path
import pf.Stdout
import http.Request

# Fetch Tom Harte's SingleStepTests (65x02 / nes6502) into
# check/single-step/data/ — pure Roc, no curl required.
#
#   roc check/single-step/fetch.roc
#
# Already-present files are skipped, so an interrupted fetch resumes.

base_url : Str
base_url = "https://raw.githubusercontent.com/SingleStepTests/65x02/main/nes6502/v1"

data_dir : Str
data_dir = "check/single-step/data"

hex2 : U64 -> Str
hex2 = |n| {
    digit = |d| if d < 10 { d.plus(48) } else { d.plus(87) } # '0'.. / 'a'..
    hi = n.to_u8_wrap().shr_zf_wrap(4).to_u64()
    lo = n.bitwise_and(15)
    Str.from_utf8([digit(hi).to_u8_wrap(), digit(lo).to_u8_wrap()]) ?? "??"
}

fetch_one! = |opcode| {
    name = hex2(opcode)
    path = Path.from_os_str(OsStr.from_str("${data_dir}/${name}.json"))
    if path.is_file!() ?? Bool.False {
        Stdout.line!("${name}.json: already present")
    } else {
        response = Http.send!(Request.from_method(GET).with_uri("${base_url}/${name}.json"))?
        if response.status() == 200 {
            path.write_bytes!(response.body())?
            Stdout.line!("${name}.json: fetched (${response.body().len().to_str()} bytes)")
        } else {
            Stdout.line!("${name}.json: HTTP ${response.status().to_str()}")?
            Err(FetchFailed(name))
        }
    }
}

fetch_all! = |opcode|
    if opcode > 255 {
        Ok({})
    } else {
        fetch_one!(opcode)?
        fetch_all!(opcode.plus(1))
    }

main! : List(OsStr) => Try({}, _)
main! = |_args| {
    fetch_all!(0)?
    Stdout.line!("all 256 opcode files present in ${data_dir}/")
}

expect hex2(0) == "00"
expect hex2(0xA9) == "a9"
expect hex2(255) == "ff"
