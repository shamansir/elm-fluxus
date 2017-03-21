import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import Fluxus.Program as Fx
import Fluxus.State exposing (..)

main : Fx.FluxusProgram
main =
    Fx.everyFrame
        [ color (vec3 1 0 0)
        , drawCube
        , translate (vec3 2 0 0)
        , scale (vec3 0.5 0.5 0.5)
        , rotate (vec3 0 45 0)
        , color (vec3 0 0.5 1)
        , drawCube
        ]
