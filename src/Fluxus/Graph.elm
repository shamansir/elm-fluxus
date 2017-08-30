module Fluxus.Graph exposing
    ( Graph
    , Leaf
    , Instance(..)
    , init
    , empty
    , join
    , add
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

type Instance = Null | Solid Geometry | Textured Geometry TextureId | Colored Geometry Vec3

type Invalidate = None | All | Some (List NodeId)

type alias Node =
    { instance : Instance
    , entity : Maybe Entity
    }

type Leaf =
    Leaf { node : NodeId
         , children : List Leaf
         , parent : Maybe NodeId
         }

type alias Graph =
    { nodes : Dict NodeId Node
    , root : Leaf
    , cursor : NodeId
    , invalidate: Invalidate
    -- , cursor: Maybe Leaf
    }

empty : Graph
empty =
    let
        rootId = 0
        rootNode =
            { instance = Null
            , entity = Nothing
            }
        nodes =  Dict.empty |> Dict.insert rootId rootNode
        rootLeaf = Leaf
            { node = rootId
            , children = []
            , parent = Nothing
            }
    in
        { nodes = Dict.empty
        , root = rootLeaf
        , cursor = rootId
        , invalidate = None
        }

init : Graph
init = empty

add : Instance -> Graph -> Graph
add instance graph  =
    graph -- FIXME: implement
     -- We need State here (it has Uniforms for Entity creation),
     -- if we want to create Entity here (may be we don't need it here, but in State?)
     -- But if we use it, we have a recursive dependency
     -- ...actually, State is the external thing, for the Node we just need
     -- MeshId, TextureId and the first variant of Entity

-- traverse a : Graph -> (Node -> a) -> List a

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
    graph.root |> flattenLeaf graph

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
                        maybeEntity = node.entity
                    in
                        case maybeEntity of
                            Just entity -> [ entity ] ++ (def.children |> flattenLeaves graph)
                            Nothing -> (def.children |> flattenLeaves graph)
                Nothing -> []
