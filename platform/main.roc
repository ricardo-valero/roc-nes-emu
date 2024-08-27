platform "emulator"
    requires {} { main : Effect U64 }
    exposes []
    packages {}
    imports [Effect.{ Effect }]
    provides [mainForHost]

mainForHost : Effect U64 as Fx
mainForHost = main
