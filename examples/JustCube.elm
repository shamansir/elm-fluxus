import Fluxus.Primitive as Primitive exposing (..)

import Fluxus.Program as Fx

import Fluxus.Primitive exposing (..)

main : Fx.FluxusProgram
main =
    Fx.everyFrame
        (\state ->
          [ state |> drawCube ])
