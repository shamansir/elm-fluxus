import Fluxus.Primitive as Primitive exposing (..)

import Fluxus.Program as Fx

main : Fx.FluxusProgram
main =
    Fx.everyFrame
        (\_ -> [ build crate ])
