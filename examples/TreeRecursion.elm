import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import Fluxus.Program as Fx
import Fluxus.State exposing (..)
import Fluxus.Primitive as Primitive exposing (..)

drawRow : Int -> State -> State
drawRow count state =
    if (count > 0) then
        state
            |> translate (vec3 4 0 0)
            |> drawCube
            |> rotate (vec3 (10 * (sin (time state))) 0 0)
            |> withState (\state -> 
                 state
                 |> rotate (vec3 0 25 0)
                 |> drawRow (count - 1)
               )     
            |> withState (\state -> 
                 state
                 |> rotate (vec3 0 -25 0)
                 |> drawRow (count - 1)
               )
    else
        state

main : Fx.FluxusProgram
main =
    Fx.everyFrame (drawRow 10)
