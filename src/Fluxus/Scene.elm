module Fluxus.Scene exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import Fluxus.Primitive as Primitive exposing (..)

type alias Person =
    { position : Vec3
    , velocity : Vec3
    }

type alias Scene =
    { person: Person
    , size: (Float, Float)
    , primitives: List Primitive }
