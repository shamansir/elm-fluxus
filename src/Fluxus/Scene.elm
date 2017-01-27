module Fluxus.Scene exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import Fluxus.Primitive as Primitive exposing (..)

import Keyboard
import Window

type alias Person =
    { position : Vec3
    , velocity : Vec3
    }

type alias Scene =
    { renderers: List Renderer
    , person: Person
    , size: Window.Size
    , keys: Keys

    }

type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , space : Bool
    }

type alias Renderer = (Float -> List Primitive)

addRenderer : Scene -> Renderer -> Scene
addRenderer scene renderer =
    { scene | renderers = renderer :: scene.renderers }

eyeLevel : Float
eyeLevel =
    2

keyFunc : Bool -> Keyboard.KeyCode -> Keys -> Keys
keyFunc on keyCode keys =
    case keyCode of
        32 ->
            { keys | space = on }

        37 ->
            { keys | left = on }

        39 ->
            { keys | right = on }

        38 ->
            { keys | up = on }

        40 ->
            { keys | down = on }

        _ ->
            keys


move : Keys -> Person -> Person
move { left, right, up, down, space } person =
    let
        direction a b =
            if a == b then
                0
            else if a then
                1
            else
                -1

        vy =
            if space then
                2
            else
                Vec3.getY person.velocity
    in
        if Vec3.getY person.position <= eyeLevel then
            { person
                | velocity =
                    vec3 (direction left right) vy (direction up down)
            }
        else
            person


physics : Float -> Person -> Person
physics dt person =
    let
        position =
            Vec3.add person.position (Vec3.scale dt person.velocity)
    in
        { person
            | position =
                if Vec3.getY position < eyeLevel then
                    Vec3.setY eyeLevel position
                else
                    position
        }


gravity : Float -> Person -> Person
gravity dt person =
    if Vec3.getY person.position > eyeLevel then
        { person
            | velocity =
                Vec3.setY
                    (Vec3.getY person.velocity - 2 * dt)
                    person.velocity
        }
    else
        person

sceneWithACrate : Scene
sceneWithACrate =
      { renderers = [ (\_ -> [ Primitive.build crate ]) ]
      , person = Person (vec3 0 eyeLevel -10) (vec3 0 0 0)
      , keys = Keys False False False False False
      , size = Window.Size 0 0
      }

-- scene : Window.Size -> Person -> List Object -> List Texture -> List Entity
-- scene { width, height } person objects textures =
--     let
--         perspective =
--             Mat4.mul
--                 (Mat4.makePerspective 45 (toFloat width / toFloat height) 0.01 100)
--                 (Mat4.makeLookAt person.position (Vec3.add person.position Vec3.k) Vec3.j)
--     in
--         List.map (toEntity perspective) objects
