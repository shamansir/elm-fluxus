module Fluxus.Graph exposing
    ( Graph
    , Leaf
    , init
    , empty
    , join
    , addMesh
    , flatten
    )

import Dict exposing (..)

import WebGL exposing (Entity, Mesh)

import Fluxus.Texture exposing (..)
import Fluxus.Link exposing (Uniforms, Vertex)

type alias NodeId = Int

type Leaf =
    Leaf { node: NodeId
         , children: List Leaf
         , parent: NodeId
         }

type alias Graph =
    { nodes : Dict NodeId ( Mesh, TextureId, Entity )
    , root : Maybe Leaf
    -- , cursor: Maybe Leaf
    }

empty : Graph
empty =
    { nodes = Dict.empty
    , root = Nothing
    }

init : Graph
init = empty

addMesh : Uniforms -> Mesh Vertex -> Graph -> Graph
addMesh uniforms mesh graph  =
    graph -- FIXME: implement

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
                        ( _, _, entity ) = node
                    in
                        [ entity ] ++ (def.children |> flattenLeaves graph)
                Nothing -> []
