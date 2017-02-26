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

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import WebGL exposing (Mesh, Texture)

import Fluxus.Core as Core exposing (toRadians)
import Fluxus.Link as Link exposing (Uniforms, Vertex)
import Fluxus.Form as Form exposing (Form)
import Fluxus.Primitive as Primitive exposing (..)

-- type StateAction = Modify (State -> State) | RegisterMesh MeshId

type Msg = LoadTexture String

type alias State =
    { time: Float
    , delta: Float
    , color: Vec3
    , transform: Mat4
    , perspective: Mat4
    , graph: Graph
    }

type alias Graph =
    { forms: List Form
    -- { root: Maybe Form
    , meshes: List (Mesh Vertex)
    , textures: List Texture
    }

init : State
init =
    { time = 0
    , delta = 0
    , color = (vec3 1 1 1)
    , transform = Mat4.identity
    , perspective = Mat4.identity
    , graph = initGraph
    }

initGraph : Graph
initGraph =
    { forms = []
    -- { root = Maybe.Nothing
    , meshes = []
    , textures = []
    }

-- applyColor : Vec3 -> State -> State
-- applyColor newColor state =
--     { state | color = newColor }

-- color : Vec3 -> StateAction
-- color newColor state =
--     Modify (applyColor newColor)

-- type StateAction = Sync (State -> State) | Async (State -> (State, Cmd Msg))

-- color : Vec3 -> StateAction -> State -> State

color : Vec3 -> State -> ( State, Cmd Msg )
color newColor state =
    { state | color = newColor }
    ! []

rotate : Vec3 -> State -> ( State, Cmd Msg )
rotate angles state =
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
        ( { outer | graph = inner.graph }, cmd )

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

storeMesh : Mesh Vertex -> Graph -> Graph
storeMesh mesh graph =
    { graph | meshes = graph.meshes ++ [ mesh ] }

-- loadMesh : Int -> State -> Mesh Vertex
-- loadMesh id state =
--     List.get id state.meshes

buildCube : State -> State
buildCube state =
    { state | graph = state.graph |> storeMesh Primitive.cube }

drawCube : State -> State
drawCube state =
    let
      stateWithNewMesh = (buildCube state)
      meshId = List.length stateWithNewMesh.graph.meshes
      newForm = { meshId = Just meshId, textureId = Maybe.Nothing }
    in
      state |> draw newForm

draw : Form -> State -> State
draw form state =
    let
      prevGraph = state.graph
      nextGraph = { prevGraph | forms = prevGraph.forms ++ [ form ] } -- loadForm
    in
      { state | graph = nextGraph }
