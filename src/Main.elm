module Main exposing (main)

{-
   Try adding the ability to crouch or to land on top of the crate.
-}

import AnimationFrame
import Html exposing (Html, text, div)
import Html.Attributes exposing (width, height, style)
import Keyboard
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task exposing (Task)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Texture as Texture exposing (Texture, Error)

import Fluxus.Scene as Scene exposing (Scene)

type alias Model =
    { scene: Scene
    }

type Msg
    = TextureLoaded (Result Error Texture)
    | KeyChange Bool Keyboard.KeyCode
    | Animate Time
    | Resize Window.Size


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

init : ( Model, Cmd Msg )
init =
    (
      { scene = Scene.sceneWithACrate
      }
    , Cmd.batch
        [ Task.attempt TextureLoaded (Texture.load "texture/wood-crate.jpg")
        , Task.perform Resize Window.size
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False)
        , Window.resizes Resize
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TextureLoaded textureResult ->
            ( { model | textures = [ Result.toMaybe textureResult ] }, Cmd.none )

        KeyChange on code ->
            ( { model | keys = keyFunc on code model.keys }, Cmd.none )

        Resize size ->
            ( { model | size = size }, Cmd.none )

        Animate dt ->
            ( { model
                | person =
                    model.person
                        |> move model.keys
                        |> gravity (dt / 500)
                        |> physics (dt / 500)
              }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view { scene, size } =
    div
        [ style
            [ ( "width", toString size.width ++ "px" )
            , ( "height", toString size.height ++ "px" )
            , ( "position", "relative" )
            ]
        ]
        [ WebGL.toHtmlWith
            [ WebGL.depth 1
            , WebGL.antialias
            ]
            [ width size.width
            , height size.height
            , style [ ( "display", "block" ) ]
            ]
            ((scene size)
            )
        , div
            [ style
                [ ( "position", "absolute" )
                , ( "font-family", "monospace" )
                , ( "color", "white" )
                , ( "text-align", "center" )
                , ( "left", "20px" )
                , ( "right", "20px" )
                , ( "top", "20px" )
                ]
            ]
            [ text message ]
        ]


message : String
message =
    "Walk around with a first person perspective.\n"
        ++ "Arrows keys to move, space bar to jump."
