module Main exposing (main)


-- Imports

import Browser
import Html exposing (Html, div, button, text, a,
                      table, tr, td)
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
        [ div [] [ table [ style  "width" "100%"
                         , style "table-layout" "fixed"
                         ]
                        -- TODO<Mike>: move styling to CSS
                         [ tr [] 
                              [ td [ style "width" "50%" ]
                                   [ drawConvexHullAlgorithmsState model ]
                              , td [ style "width" "50%" ]
                                   [ div [] [ model.step_desc ]
                                   , div [] [ button [ onClick Step ] 
                                                     [ text "next step" ]
                                            ]
                                   ]
                              ]
                         ]
                 ]
        , div [ style "text-align" "center" ] 
              [ a [ href "/about.html" ] [ text "about" ]
              ]
        ]

type alias Model =
    { polygon : Polygon
    , convex_hull_state : Polyline
    , next_step : ModelMutator
    , step_desc : Html Msg
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
    , step_desc = text "hello"
    }

-- TODO: change the type to not pass the whole mode, just the polygon and hull progress
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
