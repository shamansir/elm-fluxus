module Fluxus.Graph exposing
    ( Graph
    , Leaf
    , init
    )

import WebGL exposing (Entity)

type Leaf =
    Leaf { entity: Entity
         , meshId: Maybe Int
         , textureId: Maybe Int
         , parent: Maybe Leaf
         , children: List Leaf
         }

type alias Graph =
    { root : Maybe Leaf
    }

init : Graph
init =
    { root = Nothing
    }
