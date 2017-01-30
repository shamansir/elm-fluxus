import Fluxus.Primitive as Primitive exposing (..)

import Fluxus.Program as Fx

import Fluxus.State exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)

main : Fx.FluxusProgram
main =
    Fx.everyFrame
        (\state ->
          state
          |> colour (vec3 1 0 0)
          |> drawCube
          |> translate (vec3 2 0 0)
        --   |> scale (vec3 0.5 0.5 0.5)
        --   |> rotate (vec3 0 45 0)
        --   |> colour (vec3 0 0.5 1)
          |> drawCube
          )