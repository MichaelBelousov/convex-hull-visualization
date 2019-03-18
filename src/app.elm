module Main exposing (main)


-- Imports

import Browser
import Html exposing (Html, button, div, p, onInput)
import Html.Events exposing (onClick)
import Html.Attributesx exposing (..)
import List
import Tuple


-- Browser Model

type Msg
    = Start
    | Step
    | End

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
    , next_step : Model -> Model
    , step_description : Html
    }

-- Domain Types

type alias Point = (Float, Float)

type alias Polygon = List Point

type alias Polyline = List Point


-- App Implementation

page_state : Model

-- initial page state
page_state =
    { polygon = [(2,2), (-2,2), (-2,-2), (2,-2)]
    , convex_Hull_state = []
    , next_step = algorithm.start
    , step_description = div [] []
    }

-- TODO<Xuefeng>: this is a stub, finish and optionally rename
drawConvexHullAlgorithmsState : Model -> Html
drawConvexHullAlgorithmsState pagestate =
    div [] []
    -- returns an empty div for now


-- Browser Init

main : Program () Model Msg
main =
    Browser.sandbox
        { init = pageState
        , view = view
        , update = update
        }
