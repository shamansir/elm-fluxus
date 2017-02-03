module Fluxus.Core exposing (..)

import Fluxus.State exposing (State)

time : State -> Float
time ( env, _ ) = env.time / 1000

delta : State -> Float
delta ( env, _ ) = env.delta / 1000

toRadians : Float -> Float
toRadians = Basics.degrees
