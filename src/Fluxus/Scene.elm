module Fluxus.Scene exposing (..)

import AnimationFrame

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Task exposing (Task)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Texture as Texture exposing (Texture, Error)

import Fluxus.Primitive as Primitive exposing (..)
import Fluxus.State as State exposing (..)

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
    , state: State
    }

type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , space : Bool
    }

-- type alias Renderer = (Float -> Float -> List Primitive)
-- type alias Renderer = (State -> List Primitive)
type alias Renderer = (State -> State)

render : Float -> Float -> Scene -> List Entity
render dt time scene =
    let
        { person, size, state } = scene
        { width, height } = size
        ( p, _ ) = state
        perspective =
            Mat4.mul
                (Mat4.makePerspective 45 (toFloat width / toFloat height) 0.01 100)
                (Mat4.makeLookAt person.position (Vec3.add person.position Vec3.k) Vec3.j)
        newState = ({ p | perspective = perspective }, [])
    in
        List.concatMap (\(p, entities) -> entities)
                       (List.map (\renderer -> renderer newState) scene.renderers)

animate : Float -> Scene -> Scene
animate dt scene =
    let
        prevState = scene.state
        (p, _) = prevState
        newTime = p.time + dt
    in
        { scene
        | person =
            scene.person
                |> move scene.keys
                |> gravity (dt / 500)
                |> physics (dt / 500)
        , state = prevState
                  |> advance dt
                  |> setEntities (render dt newTime scene)
        }

eyeLevel : Float
eyeLevel =
    2

-- unit: Float
-- unit =
--    1

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

empty : Scene
empty =
    { renderers = [ ]
    , person = Person (vec3 0 eyeLevel -10) (vec3 0 0 0)
    , keys = Keys False False False False False
    , size = Window.Size 0 0
    , state = State.init
    }

addRenderer : Renderer -> Scene -> Scene
addRenderer renderer scene =
    { scene | renderers = renderer :: scene.renderers }

type Msg
    = TextureLoaded (Result Error Texture)
    | KeyChange Bool Keyboard.KeyCode
    | Animate Time
    | Resize Window.Size
    | AddRenderer Renderer

start : Scene -> ( Scene, Cmd Msg )
start scene =
    ( scene
    , Cmd.batch
        [ Task.attempt TextureLoaded (Texture.load "texture/wood-crate.jpg")
        , Task.perform Resize Window.size
        ]
    )

update : Msg -> Scene -> ( Scene, Cmd Msg )
update action scene =
    case action of
        TextureLoaded textureResult ->
            ( scene, Cmd.none )
            -- ( { model | textures = [ Result.toMaybe textureResult ] }, Cmd.none )

        KeyChange on code ->
            ( { scene | keys = keyFunc on code scene.keys }, Cmd.none )

        Resize size ->
            ( { scene | size = size }, Cmd.none )

        Animate dt ->
            ( scene |> animate dt, Cmd.none )

        AddRenderer renderer ->
            ( scene |> addRenderer renderer
            , Cmd.none
            )


subscriptions : Scene -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False)
        , Window.resizes Resize
        ]