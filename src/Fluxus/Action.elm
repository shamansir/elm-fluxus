module Fluxus.Action exposing
    ( Action(..)
    , color
    , rotate
    , translate
    , scale
    , withState
    , drawCube
    , buildCube
    )

import Fluxus.Primitive as Primitive exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import WebGL exposing (Mesh, Texture, Entity)

import Fluxus.Core as Core exposing (toRadians)
import Fluxus.Link as Link exposing (Uniforms, Vertex, toEntity)

type Action =
      Draw (Mesh Vertex)
    | Build Primitive.PrimitiveKind
    | Transform (Mat4 -> Mat4)
    | ChangeColor Vec3
    | WithState (List Action)

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

withState : List Action -> Action
withState actions =
    WithState actions

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
    Draw (Primitive.cube |> Primitive.toMesh)

-- draw : Form -> State -> State
-- draw form state =
--     let
--       graph = state.graph
--     in
--       { state | graph = { graph | forms = graph.forms ++ [ form ] } }
