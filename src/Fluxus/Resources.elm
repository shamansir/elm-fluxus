module Fluxus.Resources exposing
    ( Resources
    , init
    , findMesh
    , findTexture
    )

import Dict exposing (Dict)

import WebGL exposing (Mesh, Texture)

type alias Resources =
    { meshes: Dict Int Mesh
    , textures: Dict Int Texture
    }

init : Resources
init =
    { meshes = Dict.empty
    , textures = Dict.empty
    }

findMesh : Int -> Resources -> Maybe Mesh
findMesh id resources =
    resources.meshes |> Dict.get id

findTexture : Int -> Resources -> Maybe Texture
findTexture id resources =
    resources.textures |> Dict.get id
