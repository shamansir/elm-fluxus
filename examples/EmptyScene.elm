import Fluxus.Program as Fx

import Fluxus.State exposing (..)

render : State -> State
render rootState = rootState

main : Fx.FluxusProgram
main =
    Fx.everyFrame render
