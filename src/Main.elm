module Main exposing (main)

-- Imports

import Browser
import Html exposing (Html, div, button, text, a,
                      table, tr, td, p, i, b, ul, li)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import List
import Tuple
import Debug
import String exposing (..)
import Svg exposing (Svg, svg, circle, polyline, polygon)
import Svg.Attributes exposing (height, width, viewBox,
                                fill, stroke, strokeWidth,
                                strokeLinecap,
                                cx, cy, r, points)


-- Browser Model

type Msg
    = StepAlgorithm
    | RightClickPoint Int
    | LeftClickEdge Int
    | GrabPoint Int
    | ReleasePoint Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        StepAlgorithm ->
            progressConvexHull model
        RightClickPoint point_idx ->
            deletePoint model point_idx
        LeftClickEdge edge_idx ->
            insertPoint model edge_idx
        GrabPoint point_idx ->
            grabPoint model point_idx
        ReleasePoint point_idx ->
            releasePoint model point_idx


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
                                   , div [] [ button [ onClick StepAlgorithm ] 
                                                     [ text "next step" ]
                                            ]
                                   ]
                              ]
                         ]
                 ]
        , div [ class "footer" ] 
              [ a [ href "about.html" ] [ text "about" ] ]
        ]

type alias Stack z = List z

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

-- Constants

    -- States
init_polygon = makeCube 20

    -- Style
point_color = "blue"
point_radius = "1"
polygon_fill = "none"
polygon_stroke = "green"
polygon_stroke_width = fromFloat 1
polygon_stroke_cap = "round"
polyline_fill = "none"
polyline_stroke = "black"
polyline_stroke_width = fromFloat 0.5
polyline_stroke_cap = "round"

-- UI Strings

intro : Html Msg
intro = p
        []
        [ text ("Welcome. Together, we're going to find the convex hull of this polygon "
             ++ "on the left. If you don't know what that is, Wikipedia and Google probably "
             ++ "still exist.")
        , ul []
             [ li [] [ text "Click and drag on points to move them around"]
             , li [] [ text "Right click on a point to delete it"]
             , li [] [ text "Click and drag on edges to add points"]
             ]
        ]

-- Utilities

makeCube : Float -> Polygon
makeCube half_sz = 
    [ (half_sz, half_sz)
    , (-half_sz, half_sz)
    , (-half_sz, -half_sz)
    , (half_sz, -half_sz) ]

-- Interactions

deletePoint : Model -> Int -> Model
deletePoint model point_idx =
    let
        point = trust <| nth point_idx model.polygon
    in
        if List.length model.polygon > 3
        then { model | polygon = List.filter (\p -> p /= point) model.polygon }
        else model


insertPoint : Model -> Int -> Model
insertPoint model edge_idx =
    model

grabPoint : Model -> Int -> Model
grabPoint model point_idx =
    model

releasePoint : Model -> Int -> Model
releasePoint model point_idx =
    model

-- initial page state

    -- state when the algorithm starts
start_state : Model
start_state =
    { polygon = init_polygon
    , stack = [0,1]
    , next_point = 2
    , step_desc = text "WELCOME"
    , step_log = []
    }

    -- state when the app starts
before_start_state : Model
before_start_state =
    { polygon = init_polygon
    , stack = []
    , next_point = -1
    , step_desc = intro
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
            
-- trust that a Maybe is fulfilled
trust : Maybe a -> a
trust x = 
    case x of
        Just y -> y
        Nothing -> Debug.todo "trust got Nothing"

drawConvexHullAlgorithmsState : Model -> Html Msg
drawConvexHullAlgorithmsState model =
    let 
        svgBase extra =
            div [] [ svg [ width "800"
                         , height "600"
                         , viewBox "-40 -30 80 60"
                         ]
                         (
                         [ drawPolygon model
                         , drawPolyline model
                         ] ++ extra
                         )
                    ]
    in
        if model == before_start_state
        then svgBase []
        else svgBase [ drawNextPoint <| trust <| nth model.next_point model.polygon ]

-- Draw the polygon, return svg message    
drawPolygon : Model -> Svg msg
drawPolygon model = 
    svg []
        (
        [ polygon [ fill polygon_fill
                  , stroke polygon_stroke
                  , strokeWidth polygon_stroke_width
                  , strokeLinecap polygon_stroke_cap
                  , points (mapToSvg model.polygon)
                  ]
                  []
        ]
        ++ List.map
                (\(x,y) -> 
                circle [ fill point_color
                       , cx <| fromFloat x
                       , cy <| fromFloat y
                       , r point_radius
                       ] []
                )
                model.polygon
        )
 
calcHullProgressPolyline : Model -> Polyline
calcHullProgressPolyline model =
    model.stack
    |> List.map (\n -> trust <| nth n model.polygon)


-- Draw every polyline, return svg message
drawPolyline : Model -> Svg msg
drawPolyline model =
    polyline [ fill polyline_fill
             , stroke polyline_stroke
             , strokeWidth polyline_stroke_width
             , strokeLinecap polyline_stroke_cap
             , points (mapToSvg (calcHullProgressPolyline model))
             ]
             []


-- Draw next point in each step, return svg message
drawNextPoint : Point -> Svg msg
drawNextPoint (x,y) =
    circle [ fill point_color
           , cx (fromFloat x)
           , cy (fromFloat y)
           , r point_radius
           ]
           []


-- Mapping the list of points into svg attributes value
mapToSvg : List Point-> String
mapToSvg listPoint =
    listPoint
        |> List.map pointToString
        |> join " " 


-- Mapping point tuple into string
pointToString : Point -> String
pointToString (x, y) =
   fromFloat x
   ++ ","
   ++ fromFloat y


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
        { init = before_start_state
        , view = view
        , update = update
        }
