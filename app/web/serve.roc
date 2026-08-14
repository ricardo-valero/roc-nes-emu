app [Context, program] {
    pf: platform "https://github.com/roc-lang/basic-webserver/releases/download/0.16.0/42jC1JT3auhHSmv2Ah8mW5F2MXiAakq1UQQ4NQceQjXw.tar.zst",
    http: "https://github.com/roc-lang/http/releases/download/1.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
}

import pf.Server
import pf.Path
import pf.OsStr
import http.Response

# Pure-Roc static file server for the web app, run from the repo root:
#
#   roc app/web/serve.roc     # then open http://localhost:8642/
#
# Serves app/web/ (index.html, play.wasm, lib/*, play.nes) through the
# platform's declared file root - MIME types, caching, and path safety are
# the host's. Replaces `python3 -m http.server` per project convention.

Context : { files : Server.FileRoot }

program = { init!, respond!, shutdown! }

init! : () => Try({ config : Server.Config, context : Context }, [Exit(I64), ..])
init! = || {
    files = Server.file_root({ id: "web", path: Path.from_os_str(OsStr.from_str("app/web")) })
    config =
        Server.default_config
            .with_listen({ host: "127.0.0.1", port: 8642 })
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
