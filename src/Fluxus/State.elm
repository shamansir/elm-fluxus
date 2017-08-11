module Fluxus.State exposing
    ( State
    , init
    , withPerspective
    , dispatch
    , dispatchWithGraph
    , toUniforms
    --, (☼)
    )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import WebGL exposing (Mesh, Texture, Entity)

import Fluxus.Core as Core exposing (toRadians)
import Fluxus.Link as Link exposing (Uniforms, Vertex, toEntity)
import Fluxus.Graph as Graph exposing (..)
import Fluxus.Action exposing (..)

type alias TextureId = Int

type alias State =
    { color: Vec3
    , transform: Mat4
    , perspective: Mat4
    }

-- (☼) : Action -> ( State, Graph ) -> ( State, Graph )
-- (☼) action context =
--     dispatchOne action context

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
        Draw mesh -> ( state, graph |> Graph.addMesh (toUniforms state) mesh )
        Transform fn -> ( { state | transform = fn state.transform }, graph )
        Build primitiveKind -> ( state, graph ) -- FIXME: implement
        WithState actions ->
            let
                innerGraph = graph |> dispatchWithGraph actions state
            in
                ( state, Graph.join graph innerGraph )

toUniforms : State -> Uniforms
toUniforms state =
    { color = state.color
    , transform = state.transform
    , perspective = state.perspective
    }

cubeMeshId : Int
cubeMeshId = 0

-- should be private and `next` should be public

withPerspective : Mat4 -> State -> State
withPerspective perspective state =
    { state | perspective = perspective }
