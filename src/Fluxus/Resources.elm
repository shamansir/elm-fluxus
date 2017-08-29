module Fluxus.Resources exposing
    ( Resources
    , init
    , findMesh
    , findTexture
    )

import Dict exposing (Dict)

import WebGL exposing (Mesh, Texture)

import Fluxus.Link exposing (Vertex)

type alias Resources =
    { meshes: Dict Int (Mesh Vertex)
    , textures: Dict Int Texture
    }

init : Resources
init =
    { meshes = Dict.empty
    , textures = Dict.empty
    }

findMesh : Int -> Resources -> Maybe (Mesh Vertex)
findMesh id resources =
    resources.meshes |> Dict.get id

findTexture : Int -> Resources -> Maybe Texture
findTexture id resources =
    resources.textures |> Dict.get id
