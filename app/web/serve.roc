app [Context, program] {
    pf: platform "https://github.com/roc-lang/basic-webserver/releases/download/0.16.0/42jC1JT3auhHSmv2Ah8mW5F2MXiAakq1UQQ4NQceQjXw.tar.zst",
    http: "https://github.com/roc-lang/http/releases/download/1.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
}

import pf.Server
import pf.Path
import pf.OsStr
import pf.Cmd
import pf.Stdout
import http.Response

# Pure-Roc static file server for the web app:
#
#   roc app/web/serve.roc -- --port 8642 --dir app/web
#
# Both flags are optional (defaults above). Serves the directory through the
# platform's declared file root - MIME types, caching, and path safety are
# the host's. Replaces `python3 -m http.server` per project convention.
#
# NOTE: basic-webserver 0.16.0 exposes no argv effect, so the flags are read
# by asking the OS for our own command line: a spawned `sh`'s parent is this
# process, so `ps -o args= -p $PPID` prints it. Works interpreted (argv is
# roc's, flags still present after `--`) and compiled; unix-only, like `ps`.

Context : { files : Server.FileRoot }

program = { init!, respond!, shutdown! }

parse_dec : List(U8), U64, U64 -> U64
parse_dec = |b, i, acc|
    match b.get(i) {
        Ok(c) =>
            if c >= 48 and c <= 57 {
                parse_dec(b, i.plus(1), acc.shl_wrap(3).plus(acc.shl_wrap(1)).plus(c.minus(48).to_u64()))
            } else {
                acc
            }

        Err(_) => acc
    }

tokens_from : List(U8) -> List(Str)
tokens_from = |bytes| {
    step : U64, List(U8), List(Str) -> List(Str)
    step = |i, cur, acc|
        match bytes.get(i) {
            Ok(c) =>
                if c == 32 or c == 9 or c == 10 or c == 13 {
                    if cur.len() > 0 {
                        step(i.plus(1), [], acc.append(Str.from_utf8_lossy(cur)))
                    } else {
                        step(i.plus(1), [], acc)
                    }
                } else {
                    step(i.plus(1), cur.append(c), acc)
                }

            Err(_) =>
                if cur.len() > 0 {
                    acc.append(Str.from_utf8_lossy(cur))
                } else {
                    acc
                }
        }
    step(0, [], [])
}

flag_value : List(Str), Str -> [None, Some(Str)]
flag_value = |tokens, name| {
    find : U64 -> [None, Some(Str)]
    find = |i|
        match tokens.get(i) {
            Ok(t) =>
                if t == name {
                    match tokens.get(i.plus(1)) {
                        Ok(v) => Some(v)
                        Err(_) => None
                    }
                } else {
                    find(i.plus(1))
                }

            Err(_) => None
        }
    find(0)
}

own_argv! : () => List(Str)
own_argv! = || {
    result = Cmd.new_str("sh").args_str(["-c", "ps -o args= -p $PPID"]).exec_output!()
    match result {
        Ok(out) => tokens_from(out.stdout_utf8.to_utf8())
        Err(_) => []
    }
}

init! : () => Try({ config : Server.Config, context : Context }, [Exit(I64), ..])
init! = || {
    argv = own_argv!()
    port_raw = match flag_value(argv, "--port") {
        Some(s) => parse_dec(s.to_utf8(), 0, 0)
        None => 0
    }
    port = if port_raw > 0 and port_raw < 65536 { port_raw.to_u16_wrap() } else { 8642 }
    dir = match flag_value(argv, "--dir") {
        Some(s) => s
        None => "app/web"
    }
    Stdout.line!("serving ${dir} at http://127.0.0.1:${port.to_str()}/") ?? {}
    files = Server.file_root({ id: "web", path: Path.from_os_str(OsStr.from_str(dir)) })
    config =
        Server.default_config
            .with_listen({ host: "127.0.0.1", port: port })
            .with_file_roots([files])
    Ok({ config: config, context: { files: files } })
}

shutdown! = |_, _| Ok({})

respond! = |request, context| {
    match request.target() {
        Resource(resource) => {
            rel =
                if resource.raw_path == "/" {
                    "index.html"
                } else {
                    resource.raw_path.drop_prefix("/")
                }
            match Server.relative_file(rel) {
                Ok(relative) =>
                    Ok(
                        ServeFile({
                            files: context.files,
                            relative: relative,
                            disposition: Inline,
                            cache: Inherit,
                        }),
                    )

                Err(_) => Ok(Server.respond(Response.from_status(404)))
            }
        }

        _ => Ok(Server.respond(Response.from_status(400)))
    }
}
