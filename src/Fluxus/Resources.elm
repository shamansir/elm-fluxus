module Fluxus.Resources exposing
    ( Resources
    , init
    )

import WebGL exposing (Mesh, Texture)

import Fluxus.Link exposing (Vertex)

type alias Resources =
    { meshes: List (Mesh Vertex)
    , textures: List Texture
    }

init : Resources
init =
    { meshes = []
    , textures = []
    }
