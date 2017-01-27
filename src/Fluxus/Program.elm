module Fluxus.Program exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (width, height, style)

import WebGL

import Fluxus.Scene as Scene

type alias Msg = Scene.Msg

type alias Model = Scene.Scene

type alias FluxusProgram = Program Never Model Msg

run : FluxusProgram
run = Html.program
        { init = init
        , view = view
        , subscriptions = Scene.subscriptions
        , update = Scene.update
        }

init : ( Model, Cmd Msg )
init =
    Scene.start Scene.sceneWithACrate

-- View


view : Model -> Html Msg
view scene =
    let
        { size, time, entities } = scene
    in
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
                entities
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

