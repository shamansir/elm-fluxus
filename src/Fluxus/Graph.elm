module Fluxus.Graph exposing
    ( Graph
    , Leaf
    , Instance(..)
    , init
    , empty
    , join
    --, dive
    , addAtCursor
    , flatten
    )

import Dict exposing (..)

import WebGL exposing (Entity, Mesh)
import Math.Vector3 as Vec3 exposing (Vec3)

import Fluxus.Link exposing (Uniforms, Vertex, vertexShader, fragmentShader)
import Fluxus.Resources exposing (..)

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
         , parent : Maybe Leaf
         }

type alias Graph =
    { nodes : Dict NodeId Node
    , root : Leaf
    , cursor : Leaf
    , invalidate: Invalidate
    -- , cursor: Maybe Leaf
    }

nullNodeId : NodeId
nullNodeId = 0

empty : Graph
empty =
    let
        rootNode = nullNode
        nodes =  Dict.empty |> Dict.insert nullNodeId rootNode
        rootLeaf = Leaf
            { node = nullNodeId
            , children = []
            , parent = Nothing
            }
    in
        { nodes = Dict.empty
        , root = rootLeaf
        , cursor = rootLeaf
        , invalidate = None
        }

init : Graph
init = empty

nullNode : Node
nullNode =
    { instance = Null
    , entity = Nothing
    }

getNodeById : NodeId -> Graph -> Node
getNodeById nodeId graph =
    graph.nodes |> Dict.get nodeId

getNodeAtCursor : Graph -> Node
getNodeAtCursor graph =
    getNodeById graph.cursor.node

-- dive : Graph -> Graph
-- dive graph =
--     let
--         cursor = graph.cursor
--         branch = nullNode
--         branchLeaf =
--             { node = nullNode
--             , children = []
--             , parent = { cursor | children = cursor.children ++ [ branchLeaf ] }
--             }
--     in
--         { graph
--         | cursor = branchLeaf
--         }

addAtCursor : Instance -> Resources -> Graph -> Graph
addAtCursor instance resources graph =
    let
        cursor = graph.cursor
        nodeId = Dict.size graph.nodes
        newNode =
            { instance = instance
            , entity = State.toEntity instance resources
            }
        updatedGraph =
            { graph
            | nodes = Dict.insert nodeId newNode
            }
        newLeaf =
            { node = nodeId
            , children = []
            , parent = cursor
            }
    in
        { updatedGraph
        | cursor =
            { cursor
            | children = cursor.children ++ [ newLeaf ]
            }
        }

    -- FIXME: implement
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
                        flattenedChildren = (def.children |> flattenLeaves graph)
                    in
                        case maybeEntity of
                            Just entity -> [ entity ] ++ flattenedChildren
                            Nothing -> flattenedChildren
                Nothing -> []
