module Fluxus.State exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import Fluxus.Core exposing (toRadians)
import Fluxus.Link as Link exposing (Uniforms, Vertex)
import Fluxus.Form exposing (Form)

type alias State =
    { time: Float
    , delta: Float
    , color: Vec3
    , transform: Mat4
    , perspective: Mat4
    , forms: List Forms
    }

init : State
init =
    { time = 0
    , delta = 0
    , color = (vec3 1 1 1)
    , transform = Mat4.identity
    , perspective = Mat4.identity
    , forms = []
    }

color : Vec3 -> State -> State
color newColor state =
    { state | color = newColor }

rotate : Vec3 -> State -> State
rotate angles state =
    let
        ( angleX, angleY, angleZ ) = Vec3.toTuple angles
    in
        { state | transform = state.transform |> Mat4.rotate (toRadians angleX) (vec3 1 0 0)
                                              |> Mat4.rotate (toRadians angleY) (vec3 0 1 0)
                                              |> Mat4.rotate (toRadians angleZ) (vec3 0 0 1)
        }

-- rotate : Vec3 -> State -> State
-- rotate angles state =
--     let
--         ( angleX, angleY, angleZ ) = Vec3.toTuple angles
--     in
--         state
--             |> rotateX angleX
--             |> rotateY angleY
--             |> rotateZ angleZ

rotateX : Float -> State -> State
rotateX angleX state =
    rotateByAxis angleX (vec3 1 0 0) state

rotateY : Float -> State -> State
rotateY angleY state =
    rotateByAxis angleY (vec3 0 1 0) state

rotateZ : Float -> State -> State
rotateZ angleZ state =
    rotateByAxis angleZ (vec3 0 0 1) state

rotateByAxis : Float -> Vec3 -> State -> State
rotateByAxis angle axis state =
    { state | transform = state.transform |> Mat4.rotate (toRadians angle) axis }

translate : Vec3 -> State -> State
translate position state =
    { state | transform = state.transform |> Mat4.translate position }

scale : Vec3 -> State -> State
scale amount state =
    { state | transform = state.transform |> Mat4.scale amount }

-- should be private and `next` should be public
advance : Float -> State -> State
advance dt state =
    { state
    | delta = dt
    , time = state.time + dt
    }

next : Mat4 -> Float ->  State -> State
next perspective dt state =
    { state | perspective = perspective } |> advance dt

-- setEntities : List Entity -> State -> State
-- setEntities newEntities (env, _) =
--     ( env, newEntities )

withState : (State -> State) -> State -> State
withState fn outer =
    let
        inner = fn outer
    in
        { outer | forms = inner.forms }

time : State -> Float
time { time } = time / 1000

delta : State -> Float
delta { delta } = delta / 1000

toUniforms : State -> Uniforms
toUniforms state =
    -- state
    { color = state.color
    , transform = state.transform
    , perspective = state.perspective
    }
