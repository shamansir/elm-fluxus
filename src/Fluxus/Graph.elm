module Fluxus.Graph exposing
    ( Graph
    , Leaf
    , init
    , empty
    , addMesh
    , attach
    , join
    , flatten
    )

import WebGL exposing (Entity)

import Fluxus.Link exposing (Uniforms)
import Fluxus.Resources exposing (Resources)

type Leaf =
    Leaf { entity: Maybe Entity
         , meshId: Maybe Int
         , textureId: Maybe Int
         , parent: Maybe Leaf
         , children: Maybe (List Leaf)
         }

type alias Graph =
    { root : Leaf
    , cursor: Leaf
    }

empty : Graph
empty =
    let
        newRoot = emptyLeaf
    in
        { root = newRoot
        , cursor = newRoot
        }

init : Graph
init = empty

emptyLeaf : Leaf
emptyLeaf = Leaf
    { entity = Nothing
    , meshId = Nothing
    , textureId = Nothing
    , parent = Nothing
    , children = Nothing
    }

addMesh : Int -> Uniforms -> Graph -> Resources -> Graph
addMesh id uniforms graph resources =
    graph -- FIXME: implement

attach : List Leaf -> Graph -> Graph
attach leaves graph =
    { graph | cursor = graph.cursor |> attachToLeaf leaves }

attachToLeaf : List Leaf -> Leaf -> Leaf
attachToLeaf leaves leaf =
    case leaf of
        Leaf def ->
            case def.children of
                Nothing -> Leaf { def | children = Just leaves }
                Just children -> Leaf { def | children = Just (children ++ leaves) }

join : Graph -> Graph -> Graph
join firstGraph secondGraph =
    -- FIXME: add the contents of second graph to cursor
    secondGraph

flatten : Graph -> List Entity
flatten graph =
    flattenLeaf graph.root

flattenLeaf : Leaf -> List Entity
flattenLeaf leaf =
    case leaf of
        Leaf def ->
            case def.children of
                Nothing ->
                    case def.entity of
                        Nothing -> [ ]
                        Just entity -> [ entity ]
                Just children ->
                    case def.entity of
                        Nothing -> children |> List.concatMap flattenLeaf
                        Just entity -> [ entity ] ++ (children |> List.concatMap flattenLeaf)
