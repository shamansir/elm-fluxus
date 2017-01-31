module Fluxus.State exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import WebGL exposing (Entity)

type alias Environment =
    { color: Vec3
    , transform: Mat4
    , perspective: Mat4
    , delta: Float
    , time: Float
    }

type alias State =
    ( Environment , List Entity )

init : State
init =
    ( { color = (vec3 0 0 0)
      , transform = Mat4.identity
      , perspective = Mat4.identity
      , delta = 0
      , time = 0
      }
    , []
    )

color : Vec3 -> State -> State
color newColor ( env, entities ) =
    ( { env | color = newColor }
    , entities
    )

rotate : Float -> Vec3 -> State -> State
rotate angle axis ( env, entities ) =
    ( { env | transform = env.transform |> Mat4.rotate angle axis }
    , entities
    )

translate : Vec3 -> State -> State
translate position ( env, entities ) =
    ( { env | transform = env.transform |> Mat4.translate position }
    , entities
    )

scale : Vec3 -> State -> State
scale amount ( env, entities ) =
    ( { env | transform = env.transform |> Mat4.scale amount  }
    , entities
    )

advance : Float -> State -> State
advance dt ( env, entities ) =
    ( { env
      | delta = dt
      , time = env.time + dt }
    , entities
    )

next : Mat4 -> Float ->  State -> State
next perspective dt ( env, _ ) =
    ( { env | perspective = perspective }, [] ) |> advance dt

setEntities : List Entity -> State -> State
setEntities newEntities (env, _) =
    ( env, newEntities )
