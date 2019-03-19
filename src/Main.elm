module Main exposing (main)


-- Imports

import Browser
import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import List
import Tuple


-- Browser Model

type Msg
    = Start
    | Step

update : Msg -> Model -> Model
update msg model =
    case msg of
        Start ->
            model
        Step ->
            model

view : Model -> Html Msg
view model =
    div []
        [ text "This will be the rendered model"
        , drawConvexHullAlgorithmsState model
        ]

type alias Model =
    { polygon : Polygon
    , convex_hull_state : Polyline
    , next_step : ModelMutator
    , step_description : Html Msg
    }

-- 
type ModelMutator = ModelMutator (Model -> Model)

-- Domain Types

type alias Point = (Float, Float)

type alias Polygon = List Point

type alias Polyline = List Point


-- App Implementation


-- initial page state
page_state : Model
page_state =
    { polygon = [(2,2), (-2,2), (-2,-2), (2,-2)]
    , convex_hull_state = []
    , next_step = ModelMutator startAlgorithm
    , step_description = text "hello"
    }

-- TODO<Xuefeng>: this is a stub, finish and optionally rename
drawConvexHullAlgorithmsState : Model -> Html Msg
drawConvexHullAlgorithmsState model =
    div [] []
    -- returns an empty div for now

-- TODO<Tyler>: this is a stub, finish and optionally rename
startAlgorithm : Model -> Model
startAlgorithm model =
    model
    -- returns an unchanged model for now

-- Browser Init

main : Program () Model Msg
main =
    Browser.sandbox
        { init = page_state
        , view = view
        , update = update
        }
