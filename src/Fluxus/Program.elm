module Fluxus.Program exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (width, height, style)

import WebGL

import Fluxus.Scene as Scene
import Fluxus.State as State exposing (..)

type alias Msg = Scene.Msg
type alias Model = Scene.Model

type alias FluxusProgram = Program Never Model Msg

run : FluxusProgram
run =
    runWith Scene.noActions Scene.empty

runWith : Scene.Renderer -> Scene.Scene -> FluxusProgram
runWith renderer scene =
    Html.program
        { init = (Scene.run renderer scene)
        , view = view
        , subscriptions = Scene.subscriptions
        , update = Scene.update
        }

-- everyFrame : Renderer

everyFrame : List State.Action -> FluxusProgram
everyFrame actions =
    Scene.empty |> runWith (Scene.dispatchingRenderer actions)

-- View

view : Model -> Html Msg
view ( { size }, _, entities ) =
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

