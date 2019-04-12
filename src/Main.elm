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

type alias Stack a = List a

type alias Model =
    { polygon : Polygon
    , stack : Stack Int
    , next_point : Int
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
    , stack = [0,1]
    , next_point = 0
    , step_desc = text "hello"
    }


-- Returns the nth element or Nothing (if not exists)
nth : Int -> List a -> Maybe a
nth n list =
    case list of
        head::rest -> 
            if n==0 then Just head
                    else nth (n-1) rest
        [] -> 
            Nothing


-- Removes the last element from a List/Stack
removeLast : List a -> Maybe (List a)
removeLast list = 
    case list of
        [] ->
            Nothing
        other ->
            Just (List.reverse (Maybe.withDefault [] (List.tail (List.reverse list))))


--
ccw : Point -> Point -> Point -> Int
ccw (ax,ay) (bx,by) (cx,cy) =
    let
        value = (ax * (by - cy)) - (bx * (ay - cy)) + (cx * (ay - by))
    in
        if value > 0 then 1
        else if value < 0 then -1
        else 0
            

-- TODO: change the type to not pass the whole mode, just the polygon and hull progress
-- TODO<Xuefeng>: this is a stub, finish and optionally rename
drawConvexHullAlgorithmsState : Model -> Html Msg
drawConvexHullAlgorithmsState model =
    div []
        [
        ]
    -- returns an empty div for now


-- TODO<Tyler>: this is a stub, finish and optionally rename
-- 
progressConvexHull : Model -> Model
progressConvexHull model =
    let
        top = Maybe.withDefault (0,0) (nth (Maybe.withDefault 0 (nth 1 (List.reverse model.stack))) model.polygon)
        scd = Maybe.withDefault (0,0) (nth (Maybe.withDefault 0 (nth 2 (List.reverse model.stack))) model.polygon)
        next = Maybe.withDefault (0,0) (nth model.next_point model.polygon)
    in
        if ccw scd top next < 1 then
            { model | stack = case removeLast model.stack of
                      Nothing -> []
                      Just stack -> stack
                    , next_point = model.next_point + 1
            }
        else
            model


-- Browser Init

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initial_state
        , view = view
        , update = update
        }
