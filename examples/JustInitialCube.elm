import Fluxus.Program as Fx
import Fluxus.Action exposing (..)

main : Fx.FluxusProgram
main =
    Fx.everyFrame
        [ drawCube ]
