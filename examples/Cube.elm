import Fluxus.Primitive as Primitive exposing (..)

import Fluxus.Program as Fx

import Fluxus.State exposing (..)

main : Fx.FluxusProgram
main =
    Fx.everyFrame
        (\{ state } ->
          state
          |> colour (vector 1 0 0)
          |> drawCube
          |> translate (vector 2 0 0)
          |> scale (vector 0.5 0.5 0.5)
          |> rotate (vector 0 45 0)
          |> colour (vector 0 0.5 1)
          |> drawCube
          )
