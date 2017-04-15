module Fluxus.Resources exposing
    ( Resources
    , init
    )

import WebGL exposing (Mesh, Texture)

type alias Resources =
    { textures: List Texture
    }

init : Resources
init =
    { textures = []
    }
