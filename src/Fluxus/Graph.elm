module Fluxus.Graph exposing
    ( Graph
    , Leaf
    , Instance
    , init
    , empty
    , join
    , addMesh
    , flatten
    )

import Dict exposing (..)

import WebGL exposing (Entity, Mesh)
import Math.Vector3 as Vec3 exposing (Vec3)

import Fluxus.Link exposing (Uniforms, Vertex, vertexShader, fragmentShader)

type alias NodeId = Int
type alias MeshId = Int
type alias TextureId = Int

type alias Geometry = MeshId

type Instance = Solid Geometry | Textured Geometry TextureId | Colored Geometry Vec3

type Invalidate = None | All | Some (List NodeId)

type alias Node =
    { instance: Instance
    , entity: Entity
    }

type Leaf =
    Leaf { node: NodeId
         , children: List Leaf
         , parent: NodeId
         }

type alias Graph =
    { nodes : Dict NodeId Node
    , root : Maybe Leaf
    , invalidate: Invalidate
    -- , cursor: Maybe Leaf
    }

empty : Graph
empty =
    { nodes = Dict.empty
    , root = Nothing
    , invalidate = None
    }

init : Graph
init = empty

addMesh : Uniforms -> Mesh Vertex -> Graph -> Graph
addMesh uniforms mesh graph  =
    graph -- FIXME: implement
     -- We need State here (it has Uniforms for Entity creation),
     -- if we want to create Entity here (may be we don't need it here, but in State?)
     -- But if we use it, we have a recursive dependency

-- attach : List Leaf -> Graph -> Graph
-- attach leaves graph =
--     { graph | cursor = graph.cursor |> attachToLeaf leaves }

-- attachToLeaf : List Leaf -> Leaf -> Leaf
-- attachToLeaf leaves leaf =
--     case leaf of
--         Leaf def ->
--             case def.children of
--                 Nothing -> Leaf { def | children = Just leaves }
--                 Just children -> Leaf { def | children = Just (children ++ leaves) }

join : Graph -> Graph -> Graph
join firstGraph secondGraph =
    -- FIXME: add the contents of second graph to cursor
    secondGraph

-- findNode : NodeId -> Graph -> Node


-- findNodes : List NodeId -> Graph -> List Node
-- findNodes nodeIds graph =
--     List.map (findNode graph) nodeIds

flatten : Graph -> List Entity
flatten graph =
    graph.root
        |> Maybe.map (flattenLeaf graph)
        |> Maybe.withDefault []

flattenLeaves : Graph -> List Leaf -> List Entity
flattenLeaves graph leaves  =
    List.concatMap (flattenLeaf graph) leaves

flattenLeaf : Graph -> Leaf -> List Entity
flattenLeaf graph leaf =
    case leaf of
        Leaf def ->
            case graph.nodes |> Dict.get def.node of
                Just node ->
                    let
                        { entity } = node
                    in
                        [ entity ] ++ (def.children |> flattenLeaves graph)
                Nothing -> []
