module Fluxus.Program exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (width, height, style)

import WebGL

import Fluxus.Scene as Scene

type alias Msg = Scene.Msg
type alias Model = Scene.Model

type alias FluxusProgram = Program Never Model Msg

run : FluxusProgram
run =
    runWith Scene.empty

runWith : Scene.Scene -> FluxusProgram
runWith scene =
    Html.program
        { init = Scene.run scene
        , view = view
        , subscriptions = Scene.subscriptions
        , update = Scene.update
        }

-- everyFrame : Renderer

everyFrame : Scene.Renderer -> FluxusProgram
everyFrame renderer =
    runWith (Scene.empty |> Scene.addRenderer renderer) -- should fire a message of AddRenderer

-- View

view : Model -> Html Msg
view ( { size }, entities ) =
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

