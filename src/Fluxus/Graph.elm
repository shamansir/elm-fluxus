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

type Leaf =
    Leaf { entity: Maybe Entity
         , meshId: Maybe Int
         , textureId: Maybe Int
         , parent: Maybe Leaf
         , children: Maybe (List Leaf)
         }

type alias Graph =
    { root : Maybe Leaf
    , cursor: Maybe Leaf
    }

empty : Graph
empty =
    { root = Nothing
    , cursor = Nothing
    }

init : Graph
init = empty

addMesh : Int -> Uniforms -> Graph -> Graph
addMesh id uniforms graph =
    graph -- FIXME: implement

attach : List Leaf -> Graph -> Graph
attach leaves graph =
    case graph.cursor of
        Nothing -> case graph.root of
            Nothing -> graph -- FIXME: create empty root and attach children to it
            Just root -> { graph | root = Just (root |> attachToLeaf leaves) }
        Just cursor -> { graph | cursor = Just (cursor |> attachToLeaf leaves) }

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
    case graph.root of
        Nothing -> []
        Just rootLeaf -> flattenLeaf rootLeaf

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
