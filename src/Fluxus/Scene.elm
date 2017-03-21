module Fluxus.Scene exposing
    ( Scene
    , Renderer
    , Model
    , Msg
    , empty
    , run
    , runEmpty
    , runWithRenderer
    , runWithGraph
    , runWithActions
    , update
    , subscriptions
    , renderActions
    , renderGraph
    , noActions
    )

import Dict exposing (..)
import AnimationFrame

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Task exposing (Task)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Texture as Texture exposing (Texture, Error)

import Fluxus.Time as T exposing (TimePosition, advance)
import Fluxus.State as State exposing (..)
import Fluxus.Link exposing (Vertex, Uniforms)
import Fluxus.Graph as Graph exposing (..)
import Fluxus.Resources as Resources exposing (..)

import Keyboard
import Window

type alias Person =
    { position : Vec3
    , velocity : Vec3
    }

type alias Scene =
    { time: TimePosition
    , person: Person
    , size: Window.Size
    , keys: Keys
    , resources: Resources
    }

type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , space : Bool
    }

type alias Renderer = (State -> Graph)

type alias Model =
    ( Scene
    , Renderer
    , List Entity
    )

type Msg
    = TextureLoaded (Result Error Texture)
    | KeyChange Bool Keyboard.KeyCode
    | Animate Time
    | Resize Window.Size

-- type alias MeshId = Int

type alias Meshes = Dict Int (Mesh Vertex)

animate : Renderer -> Float -> Scene -> ( Model, Cmd Msg )
animate renderer dt scene =
    let
        { person, size } = scene
        { width, height } = size
        newTime = scene.time |> advance dt
        newPerspective =
            Mat4.mul
                (Mat4.makePerspective 45 (toFloat width / toFloat height) 0.01 100)
                (Mat4.makeLookAt person.position (Vec3.add person.position Vec3.k) Vec3.j)
        newPerson = scene.person
                |> move scene.keys
                |> gravity (dt / 500)
                |> physics (dt / 500)
        newState = State.init |> State.withPerspective newPerspective
        newEntities = (renderer newState) |> Graph.unfold
    in
        (
            (
              { scene
              | person = newPerson
              , time = newTime
              }
            , renderer
            , newEntities
            )
        , Cmd.none
        )

eyeLevel : Float
eyeLevel =
    2

-- -- unit: Float
-- -- unit =
-- --    1

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
    { time = T.init
    , person = Person (vec3 0 eyeLevel -10) (vec3 0 0 0)
    , keys = Keys False False False False False
    , size = Window.Size 0 0
    , resources = Resources.init
    }

-- toEntity : Meshes -> State -> Form -> Maybe Entity
-- toEntity meshes state form =
--     case form.meshId of
--         Just meshId ->
--             let
--                 locatedMesh = Dict.get meshes meshId
--             in
--                 case locatedMesh of
--                     Just mesh ->
--                         Just
--                             (toEntity (toUniforms state) mesh)
--                     Nothing -> Nothing
--         Nothing -> Nothing

-- -- toInitialEntity : Uniforms -> Mesh Vertex -> Maybe Entity
-- -- toInitialEntity uniforms mesh =
-- --     toEntity
-- --         uniforms
-- --         { meshId = mesh
-- --         , textureId = Maybe.Nothing
-- --         }

run : Renderer -> Scene -> ( Model, Cmd Msg )
run renderer scene =
    ( ( scene, renderer, [] )
    , Cmd.batch
        [ Task.attempt TextureLoaded (Texture.load "texture/wood-crate.jpg")
        , Task.perform Resize Window.size
        ]
    )

runEmpty : ( Model, Cmd Msg )
runEmpty =
    run noActions empty

runWithRenderer : Renderer -> ( Model, Cmd Msg )
runWithRenderer renderer =
    run renderer empty

runWithGraph : Graph -> ( Model, Cmd Msg )
runWithGraph graph =
    runWithRenderer (renderGraph graph)

runWithActions : List Action -> ( Model, Cmd Msg )
runWithActions actions =
    runWithRenderer (renderActions actions)

renderActions : List Action -> Renderer
renderActions actions =
    (\state -> state |> State.dispatch actions)

renderGraph : Graph -> Renderer
renderGraph graph =
    (\_ -> graph)

noActions : Renderer
noActions =
    (\_ -> Graph.empty)

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    ( model, Cmd.none )

-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update action ( scene, entities ) =
--     case action of
--         TextureLoaded textureResult ->
--             ( ( scene, entities ), Cmd.none )
--             -- ( { model | textures = [ Result.toMaybe textureResult ] }, Cmd.none )

--         KeyChange on code ->
--             ( ( { scene | keys = keyFunc on code scene.keys }, entities ), Cmd.none )

--         Resize size ->
--             ( ( { scene | size = size }, entities ), Cmd.none )

--         Animate dt ->
--             scene |> animate dt

--         AddRenderer renderer ->
--             (
--                 ( scene |> addRenderer renderer
--                 , entities
--                 )
--             , Cmd.none
--             )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False)
        , Window.resizes Resize
        -- , TextureLoaded (Result Error Texture)
        -- , State.meshRequests RegisterMesh
        ]
