module Fluxus.State exposing
    ( State
    , Msg
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
    , nextMesh: Int
    , forms: List Form
    }

init : State
init =
    { time = 0
    , delta = 0
    , color = (vec3 1 1 1)
    , transform = Mat4.identity
    , perspective = Mat4.identity
    , nextMesh = 0
    , forms = []
    }

type Msg = RegisterMesh (Mesh Vertex) Int

-- applyColor : Vec3 -> State -> State
-- applyColor newColor state =
--     { state | color = newColor }

-- color : Vec3 -> StateAction
-- color newColor state =
--     Modify (applyColor newColor)

color : Vec3 -> State -> ( State, Cmd Msg )
color newColor state =
    { state | color = newColor } ! []

rotate_ : Vec3 -> State -> ( State, Cmd Msg )
rotate_ angles state =
    let
        ( angleX, angleY, angleZ ) = Vec3.toTuple angles
    in
        { state | transform = state.transform |> Mat4.rotate (toRadians angleX) (vec3 1 0 0)
                                              |> Mat4.rotate (toRadians angleY) (vec3 0 1 0)
                                              |> Mat4.rotate (toRadians angleZ) (vec3 0 0 1)
        }
        ! []

-- rotate : Vec3 -> State -> State
-- rotate angles state =
--     let
--         ( angleX, angleY, angleZ ) = Vec3.toTuple angles
--     in
--         state
--             |> rotateX angleX
--             |> rotateY angleY
--             |> rotateZ angleZ

rotateX_ : Float -> State -> ( State, Cmd Msg )
rotateX_ angleX state =
    rotateByAxis angleX (vec3 1 0 0) state

rotateY : Float -> State -> ( State, Cmd Msg )
rotateY angleY state =
    rotateByAxis angleY (vec3 0 1 0) state

rotateZ : Float -> State -> ( State, Cmd Msg )
rotateZ angleZ state =
    rotateByAxis angleZ (vec3 0 0 1) state

rotateByAxis : Float -> Vec3 -> State -> ( State, Cmd Msg )
rotateByAxis angle axis state =
    { state | transform = state.transform |> Mat4.rotate (toRadians angle) axis } ! []

translate : Vec3 -> State -> ( State, Cmd Msg )
translate position state =
    { state | transform = state.transform |> Mat4.translate position } ! []

scale : Vec3 -> State -> ( State, Cmd Msg )
scale amount state =
    { state | transform = state.transform |> Mat4.scale amount } ! []

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
    let
        nextMeshId = state.nextMesh + 1
    in
        (
        { state | nextMesh = nextMeshId }
        , Cmd.batch [ cmd, Cmd (RegisterMesh cube nextMeshId) ]
        )

drawCube : State -> State
drawCube state =
    state |> draw buildCube

draw : Form -> State -> State
draw form state =
    { state
    | forms = state.forms ++ [ form ]
    }
