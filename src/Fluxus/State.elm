module Fluxus.State exposing
    ( State
    , init
    , withPerspective
    , dispatch
    , dispatchWithGraph
    , color
    , rotate
    , translate
    , scale
    , withState
    , toUniforms
    , (☼)
    )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import WebGL exposing (Mesh, Texture, Entity)

import Fluxus.Core as Core exposing (toRadians)
import Fluxus.Link as Link exposing (Uniforms, Vertex, toEntity)
import Fluxus.Primitive as Primitive exposing (..)
import Fluxus.Graph as Graph exposing (..)

type alias MeshId = Int

type alias TextureId = Int

type Action =
      Draw MeshId
    | Build (Mesh Vertex)
    | Transform (Mat4 -> Mat4)
    | ChangeColor Vec3
    | Nest (List Action)

type alias State =
    { color: Vec3
    , transform: Mat4
    , perspective: Mat4
    }

(☼) : Action -> ( State, Graph ) -> ( State, Graph )
(☼) action context =
    dispatchOne action context

init : State
init =
    { color = (vec3 1 1 1)
    , transform = Mat4.identity
    , perspective = Mat4.identity
    }

dispatch : List Action -> State -> Graph
dispatch actions state =
    Graph.init |> dispatchWithGraph actions state

dispatchWithGraph : List Action -> State -> Graph -> Graph
dispatchWithGraph actions state graph =
       actions
    |> List.foldl dispatchOne ( state, graph )
    |> Tuple.second

dispatchOne : Action -> ( State, Graph ) -> ( State, Graph )
dispatchOne action ( state, graph ) =
    case action of
        ChangeColor color -> ( { state | color = color }, graph )
        Draw meshId -> ( state, graph |> Graph.addMesh meshId (toUniforms state) )
        Transform fn -> ( { state | transform = fn state.transform }, graph )
        Build mesh -> ( state, graph ) -- FIXME: implement
        Nest actions ->
            let
                innerGraph = graph |> dispatchWithGraph actions state
            in
                ( state, Graph.join graph innerGraph )

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
rotateByAxis angle axis =
    Transform (\matrix -> matrix |> Mat4.rotate (toRadians angle) axis)

translate : Vec3 -> Action
translate position =
    Transform (\matrix -> matrix |> Mat4.translate position)

scale : Vec3 -> Action
scale amount =
    Transform (\matrix -> matrix |> Mat4.scale amount)

-- should be private and `next` should be public

withPerspective : Mat4 -> State -> State
withPerspective perspective state =
    { state | perspective = perspective }

withState : List Action -> Action
withState actions =
    Nest actions

toUniforms : State -> Uniforms
toUniforms state =
    { color = state.color
    , transform = state.transform
    , perspective = state.perspective
    }

cubeMeshId : Int
cubeMeshId = 0

-- storeMesh : Mesh Vertex -> Graph -> Graph
-- storeMesh mesh graph =
--     { graph | meshes = graph.meshes ++ [ mesh ] }

-- loadMesh : Int -> State -> Mesh Vertex
-- loadMesh id state =
--     List.get id state.meshes

-- buildCube : State -> State
-- buildCube state =
--     { state | graph = state.graph |> storeMesh Primitive.cube }

buildCube : Action
buildCube =
    Build Primitive.cube

-- drawCube : State -> State
-- drawCube state =
--     let
--       stateWithNewMesh = (buildCube state)
--       meshId = List.length stateWithNewMesh.graph.meshes
--       newForm = { meshId = Just meshId, textureId = Maybe.Nothing }
--     in
--       state |> draw newForm



drawCube : Action
drawCube =
    Draw cubeMeshId

-- draw : Form -> State -> State
-- draw form state =
--     let
--       graph = state.graph
--     in
--       { state | graph = { graph | forms = graph.forms ++ [ form ] } }
