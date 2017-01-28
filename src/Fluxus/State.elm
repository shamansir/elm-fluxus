module Fluxus.State exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

type alias State =
    { colour: Vec3
    , transform: Mat4
    , delta: Float
    , time: Float
    }

init : State
init =
    { colour = (vec3 0 0 0)
    , transform = Mat4.identity
    , delta = 0
    , time = 0
    }

colour : Vec3 -> State -> State
colour newColour state =
    { state | colour = newColour }

rotate : Float -> Vec3 -> State -> State
rotate angle axis state =
    { state | transform = state.transform |> Mat4.rotate angle axis }

translate : Vec3 -> State -> State
translate position state =
    { state | transform = state.transform |> Mat4.translate position }

scale : Vec3 -> State -> State
scale amount state =
    { state | transform = state.transform |> Mat4.scale amount  }

advance : Float -> State -> State
advance dt state =
    { state
    | delta = dt
    , time = state.time + dt }
