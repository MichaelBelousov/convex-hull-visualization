module Main exposing (main)

import Browser
import Html exposing (Html, button, div, onInput)
import Html.Events exposing (onClick)
import Html.Attributesx exposing (..)
import List
import Tuple


type alias Point = (Float, Float)

type alias Polygon = List Point

type alias Polyline = List Point

type alias Model =
    { polygon:              Polygon
    , convex_hull_state:    Polyline
    , next_step:            Model -> Model
    , step_description:     Html
    }


page_state: Model

-- initial page state
page_state =
    { polygon             = [(2,2), (-2,2), (-2,-2), (2,-2)]
    , convex_Hull_state   = []
    , next_step           = algorithm.start
    , step_description    = div [] []
    }
   
drawConvexHullAlgorithmsState: Model -> Html
drawConvexHullAlgorithmsState =
    -- TODO<Xuefeng>: this is a stub, finish and optionally rename
    div [] []
    -- returns a div for now
