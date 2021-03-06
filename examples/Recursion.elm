import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import Fluxus.Program as Fx
import Fluxus.State exposing (..)
import Fluxus.Action exposing (..)

drawRow : Int -> State -> State
drawRow count state =
    if (count > 0) then
        state
            -- |> color (vec3 (toFloat count / 10) 0 0)
            |> drawCube
            |> translate (vec3 3 0 0)
            |> drawRow (count - 1)
    else
        state

main : Fx.FluxusProgram
main =
    Fx.everyFrame (drawRow 10)
