module Main exposing (main)

-- Imports

import Browser
import Html exposing (Html, div, button, text, a,
                      table, tr, td)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import List
import Tuple
import Debug
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


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
            progressConvexHull model

view : Model -> Html Msg
view model =
    div []
        [ div [] [ table [ style  "width" "100%"
                         , style "table-layout" "fixed"
                         ]
                        -- TODO<Mike>: move styling to CSS
                         [ tr [] 
                              [ td [ style "width" "50%" ]
                                   [ div [] [ drawConvexHullAlgorithmsState model 
                                            , debugAlgorithmState model
                                            ]
                                   ]
                              , td [ style "width" "50%" ]
                                   [ div [] [ model.step_desc ]
                                   , div [] [ button [ onClick Step ] 
                                                     [ text "next step" ]
                                            ]
                                   ]
                              ]
                         ]
                 ]
        , div [ class "footer" ] 
              [ a [ href "about.html" ] [ text "about" ] ]
        ]

type alias Stack a = List a

type alias Model =
    { polygon : Polygon
    , stack : Stack Int
    , next_point : Int
    , step_desc : Html Msg
    , step_log : List String
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
    , next_point = 2
    , step_desc = text "hello"
    , step_log = []
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
    div [] [svg
               [width "800", height "600", viewBox "0 0 800 600"]
               [drawPolygon model, drawPolyline model, drawNextPoints model.next_point ]
           ]
    
    
-- Draw the polygon, return svg message    
drawPolygon : Model -> Svg msg
drawPolygon model = 
    polygon [ fill "yellow", stroke "green", strokeWidth "2", points (mapToSvg model.polygon) ][]


-- Draw every polyline, return svg message
drawPolyline : Model -> Svg msg
drawPolyline model =
    polyline [ fill "none", stroke "black", strokeWidth "2", points (mapToSvg (getCurrentPolylineState model.convex_hull_state model.polygon)) ][]


-- Draw next points in each step, return svg message
drawNextPoints : Point -> Svg msg
drawNextPoints point =
    circle [fill "blue",cx (fromFloat (first point) ) , cy (fromFloat (second point)), r "4" ][]


-- Mapping the list of points into svg attributes value
mapToSvg : List Point-> String
mapToSvg listPoint =
    listPoint
        |> List.map pointToString
        |> join " " 


-- Mapping point tuple into string
pointToString : Point -> String
pointToString point =
   fromFloat (first point) ++ "," ++ fromFloat (second point)


-- Get the list of polyline from current state
getCurrentPolylineState : List Int -> List Point -> List Point
getCurrentPolylineState list_int list_point =
    List.map (getCurrentStatePoint list_point) list_int


-- Get one point of the polyline from current state
getCurrentStatePoint : List Point-> Int -> Point
getCurrentStatePoint list_point n =
        fromJust(nth n list_point)
    

-- Return a of "Maybe a"
fromJust : Maybe a -> a
fromJust x = 
    case x of
        Just y -> y
        Nothing -> Debug.todo "Empty Input"



-- TODO<Tyler>: this is a stub, finish and optionally rename
-- 
progressConvexHull : Model -> Model
progressConvexHull model =
    let
        top = Debug.log "top: " (Maybe.withDefault (0,0) (nth (Maybe.withDefault 0 (nth 0 (List.reverse model.stack))) model.polygon))
        scd = Debug.log "scd: " (Maybe.withDefault (0,0) (nth (Maybe.withDefault 0 (nth 1 (List.reverse model.stack))) model.polygon))
        next = Debug.log "next: " (Maybe.withDefault (0,0) (nth model.next_point model.polygon))
    in
        if model.next_point >= List.length model.polygon then
            model
        else if Debug.log "ccw scd top next: " (ccw scd top next) < 1 then
            { model | stack = Debug.log "pop: " (case removeLast model.stack of
                              Nothing -> []
                              Just stack -> stack)
                    , step_log = ("Popped point: (" ++ (String.fromFloat (Tuple.first top)) ++ ", " ++ (String.fromFloat (Tuple.second top)) ++ ")") :: model.step_log
            }
        else
            { model | stack = Debug.log "push: " (model.stack ++ [model.next_point])
                    , next_point = model.next_point + 1
                    , step_log = ("Pushed point: (" ++ (String.fromFloat (Tuple.first next)) ++ ", " ++ (String.fromFloat (Tuple.second next)) ++ ")") :: model.step_log
            }


-- For displaying debug output
debugAlgorithmState : Model -> Html Msg
debugAlgorithmState model = 
    div [] [
        div [] (List.map (\s -> text (String.fromInt s)) model.stack)
        , div [](List.map (\s -> text s) model.step_log)
    ]


-- Browser Init

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initial_state
        , view = view
        , update = update
        }
