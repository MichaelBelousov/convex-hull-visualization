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
              [ a [ href "about.html" ] [ text "about" ]
              ]
        ]

type alias Stack = List

type alias Model =
    { polygon : Polygon
    , algorithm_state : { stack : Stack number }
    , step_desc : Html Msg
    }


-- Domain Types

type alias Point = (Float, Float)

type alias Polygon = List Point

type alias Polyline = List Point


-- App Implementation


-- initial page state
initial_state : Model
initial_state =
    { polygon = [(2,2), (-2,2), (-2,-2), (2,-2)]
    , algorithm_state = { stack = [0,1] }
    , step_desc = text "hello"
    }

nth : Int -> List a -> a
nth n list =
    case list of
        head::rest -> 
            if n==0 then head
                    else nth (n-1) rest
        [] -> 
            Nothing

ccw a b c =


progressConvexHull model =
    let
        top = nth 1 (List.reverse model.algorithm_state.stack)
        scd = nth 2 (List.reverse model.algorithm_state.stack)
    in
        if ccw()
        { model | algorithm_state.stack =
                    model.algorithm_state.stack }
            

-- TODO: change the type to not pass the whole mode, just the polygon and hull progress
-- TODO<Xuefeng>: this is a stub, finish and optionally rename
drawConvexHullAlgorithmsState : Model -> Html Msg
drawConvexHullAlgorithmsState model =
    div []
        [ svg []
        ]
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
        { init = initial_state
        , view = view
        , update = update
        }
