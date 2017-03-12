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

import WebGL exposing (Mesh, Texture, Entity)

import Fluxus.Core as Core exposing (toRadians)
import Fluxus.Link as Link exposing (Uniforms, Vertex, toEntity)
import Fluxus.Form as Form exposing (Form)
import Fluxus.Primitive as Primitive exposing (..)

type Action = Draw Int | Transform (Mat4 -> Mat4) | ChangeColor Vec3 | Nest (List Action)

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
    , meshes = []
    , textures = []
    }

dispatch : Action -> State -> ( State, Maybe (List Entity) )
dispatch action state =
    case action of
        ChangeColor color -> ( { state | color = color }, Nothing )
        Draw meshId -> ( state, Just (toEntity (state |> toUniforms) (state |> loadMesh meshId) ) )
        Transform fn -> ( { state | transform = fn state.transform }, Nothing )
        Nest actions -> List.concatMap dispatch actions state

color : Vec3 -> Action
color newColor = ChangeColor newColor

rotate : Vec3 -> Action
rotate angles =
    let
        ( angleX, angleY, angleZ ) = Vec3.toTuple angles
    in
        Transform (\matrix -> matrix |> Mat4.rotate (toRadians angleX) (vec3 1 0 0)
                                     |> Mat4.rotate (toRadians angleY) (vec3 0 1 0)
                                     |> Mat4.rotate (toRadians angleZ) (vec3 0 0 1))
-- compose rotateX with rotateY and rotateZ ?

rotateX : Float -> Action
rotateX angleX =
    rotateByAxis angleX (vec3 1 0 0)

rotateY : Float -> Action
rotateY angleY =
    rotateByAxis angleY (vec3 0 1 0)

rotateZ : Float -> Action
rotateZ angleZ =
    rotateByAxis angleZ (vec3 0 0 1)

rotateByAxis : Float -> Vec3 -> Action
rotateByAxis angle axis state =
    Transform (\matrix -> matrix |> Mat4.rotate (toRadians angle) axis)

translate : Vec3 -> Action
translate position =
    Transform (\matrix -> matrix |> Mat4.translate position)

scale : Vec3 -> Action
scale amount =
    Transform (\matrix -> matrix |> Mat4.scale amount)

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

withState : ( State -> List Action ) -> State -> State
withState fn outer =
    let
        inner = fn outer
    in
        { outer | graph = inner.graph }

time : State -> Float
time { time } = time / 1000

delta : State -> Float
delta { delta } = delta / 1000

toUniforms : State -> Uniforms
toUniforms state =
    { color = state.color
    , transform = state.transform
    , perspective = state.perspective
    }

storeMesh : Mesh Vertex -> Graph -> Graph
storeMesh mesh graph =
    { graph | meshes = graph.meshes ++ [ mesh ] }

loadMesh : Int -> State -> Mesh Vertex
loadMesh id state =
    List.get id state.meshes

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
      graph = state.graph
    in
      { state | graph = { graph | forms = graph.forms ++ [ form ] } }
