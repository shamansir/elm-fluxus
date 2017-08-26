module Fluxus.Resources exposing
    ( Resources
    , init
    )

import WebGL exposing (Mesh, Texture)

type alias Resources =
    { meshes: List Mesh
    , textures: List Texture
    }

init : Resources
init =
    { meshes = []
    , textures = []
    }
