module Fluxus.State exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import WebGL exposing (Entity)

type alias State =
    (
        { colour: Vec3
        , transform: Mat4
        , perspective: Mat4
        , delta: Float
        , time: Float
        }
    , List Entity
    )

init : State
init =
    ( { colour = (vec3 0 0 0)
      , transform = Mat4.identity
      , perspective = Mat4.identity
      , delta = 0
      , time = 0
      }
    , []
    )

colour : Vec3 -> State -> State
colour newColour ( p, entities ) =
    ( { p | colour = newColour }
    , entities
    )

rotate : Float -> Vec3 -> State -> State
rotate angle axis ( p, entities ) =
    ( { p | transform = p.transform |> Mat4.rotate angle axis }
    , entities
    )

translate : Vec3 -> State -> State
translate position ( p, entities ) =
    ( { p | transform = p.transform |> Mat4.translate position }
    , entities
    )

scale : Vec3 -> State -> State
scale amount ( p, entities ) =
    ( { p | transform = p.transform |> Mat4.scale amount  }
    , entities
    )

advance : Float -> State -> State
advance dt ( p, entities ) =
    ( { p
      | delta = dt
      , time = p.time + dt }
    , entities
    )

next : Mat4 -> Float ->  State -> State
next perspective dt ( prevP, _ ) =
    ( { prevP | perspective = perspective }, [] ) |> advance dt

setEntities : List Entity -> State -> State
setEntities newEntities (p, entities) =
    ( p, newEntities )
