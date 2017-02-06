module Fluxus.State exposing
    ( State
    , init
    , next
    , color
    , rotate
    , translate
    , scale
    , withState
    , time
    , delta
    , toUniforms
    )

import Task exposing (Task)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import WebGL exposing (Mesh)

import Fluxus.Core exposing (toRadians)
import Fluxus.Link as Link exposing (Uniforms, Vertex)
import Fluxus.Form exposing (Form)
import Fluxus.Primitive exposing (..)

-- type StateAction = Modify (State -> State) | RegisterMesh MeshId

type alias State =
    { time: Float
    , delta: Float
    , color: Vec3
    , transform: Mat4
    , perspective: Mat4
    , forms: List Form
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

type Msg = RegisterMesh (Mesh Vertex)

-- applyColor : Vec3 -> State -> State
-- applyColor newColor state =
--     { state | color = newColor }

-- color : Vec3 -> StateAction
-- color newColor state =
--     Modify (applyColor newColor)

color_ : Vec3 -> State -> State
color_ newColor state =
    { state | color = newColor }

color = makeStateFn color_

rotate_ : Vec3 -> State -> State
rotate_ angles state =
    let
        ( angleX, angleY, angleZ ) = Vec3.toTuple angles
    in
        { state | transform = state.transform |> Mat4.rotate (toRadians angleX) (vec3 1 0 0)
                                              |> Mat4.rotate (toRadians angleY) (vec3 0 1 0)
                                              |> Mat4.rotate (toRadians angleZ) (vec3 0 0 1)
        }

rotate = makeStateFn rotate_

-- rotate : Vec3 -> State -> State
-- rotate angles state =
--     let
--         ( angleX, angleY, angleZ ) = Vec3.toTuple angles
--     in
--         state
--             |> rotateX angleX
--             |> rotateY angleY
--             |> rotateZ angleZ

rotateX_ : Float -> State -> State
rotateX_ angleX state =
    rotateByAxis_ angleX (vec3 1 0 0) state

rotateY_ : Float -> State -> State
rotateY_ angleY state =
    rotateByAxis_ angleY (vec3 0 1 0) state

rotateZ_ : Float -> State -> State
rotateZ_ angleZ state =
    rotateByAxis_ angleZ (vec3 0 0 1) state

rotateByAxis_ : Float -> Vec3 -> State -> State
rotateByAxis_ angle axis state =
    { state | transform = state.transform |> Mat4.rotate (toRadians angle) axis }

rotateByAxis = makeStateFn2 rotateByAxis_

translate_ : Vec3 -> State -> State
translate_ position state =
    { state | transform = state.transform |> Mat4.translate position }

translate = makeStateFn translate_

scale_ : Vec3 -> State -> State
scale_ amount state =
    { state | transform = state.transform |> Mat4.scale amount }

scale = makeStateFn scale_

makeStateFn : (a -> State -> State) -> a -> ( State, Cmd Msg ) -> ( State, Cmd Msg )
makeStateFn fn param (state, cmd) =
    ( state |> fn param, cmd )

makeStateFn2 : (a -> b -> State -> State) -> a -> b -> ( State, Cmd Msg ) -> ( State, Cmd Msg )
makeStateFn2 fn param1 param2 (state, cmd) =
    ( state |> fn param1 param2, cmd )

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

withState : ( State -> ( State, Cmd Msg ) ) -> State -> ( State, Cmd Msg )
withState fn outer =
    let
        ( inner, cmd ) = fn outer
    in
        ( { outer | forms = inner.forms }, cmd )

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

buildCube : ( State, Cmd Msg ) -> ( State, Cmd Msg )
buildCube ( state, cmd ) =
    ( state |> draw { meshId = Just 0, textureId = Just 0 }, Cmd.batch [ cmd, Task.perform RegisterMesh cube ] )

drawCube : State -> State
drawCube state =
    state |> draw buildCube

draw : Form -> State -> State
draw form state =
    { state
    | forms = state.forms ++ [ form ]
    }
