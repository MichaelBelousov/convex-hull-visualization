module Main exposing (main)

-- Imports

import Browser
import Html exposing (Html, Attribute, div, button, text, a,
                      table, tr, td, p, i, b, ul, ol, li)
import Html.Events exposing (onClick, onDoubleClick, on)
import Html.Attributes exposing (..)
import Interactive
import List
import List.Extra exposing (last, splitAt)
import Tuple
import Debug
import Json.Decode as Decode
import String exposing (..)
import Svg exposing (Svg, svg, circle, polyline, polygon,
                     line, g, path, image, text_)
import Svg.Attributes exposing (height, width, viewBox, xlinkHref, id,
                                fill, stroke, strokeWidth, strokeLinecap,
                                strokeDasharray, cx, cy, r, points, d, x, y,
                                x1, y1, x2, y2)
onMouseDown message =
    on "mousedown" (Decode.succeed message)
onMouseUp message =
    on "mouseup" (Decode.succeed message)

-- Browser Model

type Msg
    = StepAlgorithm
    | DoubleClickPoint Int
    | LeftClickEdge Int
    | GrabPoint Int
    | ReleasePoint
    | Restart
    | InteractiveMsg Interactive.Msg

type alias InteractiveModel = (Model, Cmd Msg)

update : Msg -> Model -> InteractiveModel
update msg model =
    let
        grabbed_moved =
            case model.grabbed of
                Just grabbed ->
                     { model
                       | polygon = List.indexedMap
                                       (\i p -> if i == grabbed
                                                then windowToSvgSpace model model.interactive.mouse
                                                else p)
                                       model.polygon
                     }
                Nothing ->
                    model
        nocmd model_ = (model_, Cmd.none)
    in
    case msg of
        InteractiveMsg subMsg ->
            interactiveUpdate grabbed_moved subMsg
        StepAlgorithm ->
            nocmd <| progressConvexHull grabbed_moved
        DoubleClickPoint point_idx ->
            nocmd <| deletePoint model point_idx
        LeftClickEdge edge_idx ->
            let
                insert_done = insertPoint grabbed_moved edge_idx
            in
                nocmd <| { insert_done | grabbed = Just (edge_idx+1) }
        GrabPoint point_idx ->
            nocmd <| { grabbed_moved | grabbed = Just point_idx }
        ReleasePoint ->
            nocmd <| { grabbed_moved | grabbed = Nothing }
        Restart ->
            { before_start_state
              | polygon = model.polygon }


interactiveUpdate : Model -> Interactive.Msg -> InteractiveModel
interactiveUpdate model subMsg =
    let
        nocmd model_ = (model_, Cmd.none)
    in
    ( { model
        | interactive = Tuple.first (Interactive.update subMsg model.interactive)
      }
    , Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions _ = 
    Sub.map InteractiveMsg Interactive.subMouse

view : Model -> Browser.Document Msg
view model =
    let 
        (btn_action, btn_label) = case model.progress_state of
            NotStartedYet ->
                (StepAlgorithm, "start!")
            InProgress ->
                (StepAlgorithm, "next step")
            Done -> 
                (Restart, "restart")
    in
    { title = app_title
    , body = [
    div []
        [ div [] [ table [ style  "width" "100%"
                         , style "table-layout" "fixed"
                         ]
                         [ tr [] 
                              [ td [ class "visualization" ]
                                   [ div [] [ drawConvexHullAlgorithmsState <| flipCartesian model ]
                                   , div [ class "next-btn-container" ]
                                         [ button [ onClick btn_action ] 
                                                  [ text btn_label ]
                                         ]
                                   ]
                              , td [ class "description" ]
                                   [ div [ class "progress-log" ] model.progress_log
                                   ]
                              ]
                         ]
                 ]
        , div [ class "footer" ] 
              [ a [ href "about.html" ] [ text "about" ] ]
        ]
    ]}

type ModelState
    = NotStartedYet
    | InProgress
    | Done

type alias Model =
    { polygon : Polygon
    , stack : Stack Int
    , progress_state : ModelState
    , next_point : Int
    , grabbed : Maybe Int
    , progress_log : List (Html Msg)
    , interactive : Interactive.Model
    }


windowToSvgSpace : Model -> Point -> Point
windowToSvgSpace model (x,y) =
    let
        _ = Debug.log "window" (x,y)
        _ = Debug.log "polygon" <| trust <| nth (trust model.grabbed) model.polygon
    in
    -- FIXME: this is hackery, but I consider it elm's fault
    -- I think I need to get the live size of the SVG object and it's position,
    -- then calculate
    ( x / 10 - 40
    , y / -10 + 30
    )


-- Domain Types

type alias Point = (Float, Float)
type alias Polygon = List Point
type alias Polyline = List Point
type alias Stack z = List z

-- Constants
    
    -- App Constants
app_title = "Polygon Convex Hull"

    -- States
init_polygon = makeCube 15

    -- Style
point_color = "blue"
point_radius = fromFloat 2
next_point_color = "yellow"
polygon_fill = "none"
polygon_stroke = "blue"
polygon_stroke_width = fromFloat 1.5
polygon_stroke_cap = "round"
polyline_fill = "none"
polyline_stroke = "red"
polyline_stroke_width = fromFloat 2
polyline_stroke_cap = "round"
ccw_triangle_fill = "none"
ccw_triangle_stroke = "yellow"
ccw_triangle_stroke_width = fromFloat 0.7
ccw_triangle_stroke_dash = "3,2"
ccw_wheel_radius = 5
ccw_wheel_id = "ccw_wheel"

-- NOTE: generate z-order constants from a priority list?

-- UI Strings

intro : Html Msg
intro = p
        []
        [ text ("Welcome. Together, we're going to find the convex hull of this simple polygon "
             ++ "on the left. If you don't know what that is, Wikipedia and Google probably "
             ++ "still exist.")
        , ul []
             [ li [] [ text "Click and drag on points to move them around"]
             , li [] [ text "Double click on a point to delete it"]
             , li [] [ text "Click and drag on edges to add points"]
             ]
        ]

-- TODO: push these guys into some managed list of content that expands as the algorithm goes, with good scrolling
started_desc : Html Msg
started_desc =
    div
        []
        [ p []
            [ text "Since we're given a "
            , i [] [ text "simple polygon" ]
            , text (" our points are ordered by the edges they connect to, and by simplicity "
                 ++ "they don't overlap each other. "
                 ++ "Our simple polygon is already sorted in counter-clockwise order "
                 ++ "(if it weren't we'd just reverse it), so we'll just find the "
                 ++ "bottom-leftmost point and shift the polygon list to start at that point.")
            ]
        , p []
            [ text ("To start, we put the first two points of our polygon in a stack, "
                 ++ "and we start considering the remaining points in order. The point "
                 ++ "we're considering is in green, and the triangle of CCW comparison is "
                 ++ "the dashed red one")
            ]
        ]

-- Utilities

makeCube : Float -> Polygon
makeCube half_sz = 
    [ (-half_sz,  -half_sz)
    , (half_sz, -half_sz)
    , (half_sz, half_sz)
    , (-half_sz, half_sz) ]

    -- flip the cartesian points in the model to SVG
flipCartesian : Model -> Model
flipCartesian model =
    { model
      | polygon = List.map (\(x,y) -> (x,-y))
                           model.polygon
    }

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
    let
        (front, back) = splitAt (edge_idx+1) model.polygon
        mdpt = case (last front, List.head back) of
                (Just x, Just y) -> 
                    polygonMidPoint [x, y]
                (Just x, Nothing) -> 
                    polygonMidPoint [ trust <| List.head front
                                    , trust <| last front
                                    ]
                (Nothing, Just y) -> 
                    polygonMidPoint [ trust <| List.head back
                                    , trust <| last back
                                    ]
                _ -> Debug.todo "bad polygon"
    in
    { model | polygon = front ++ [mdpt] ++ back }

-- initial page state

    -- state when the app starts
before_start_state : InteractiveModel
before_start_state =
    let
        (subModel, subCmd) = Interactive.init
    in
    ( { polygon = init_polygon
      , stack = []
      , progress_state = NotStartedYet
      , next_point = -1
      , grabbed = Nothing
      , progress_log = [intro]
      , interactive = subModel
      }
    , Cmd.map InteractiveMsg subCmd
    )

-- Returns the nth element or Nothing (if not exists)
nth : Int -> List a -> Maybe a
nth n list =
    case list of
        head::rest -> 
            if n==0 then Just head
                    else nth (n-1) rest
        [] -> 
            Nothing

-- CCW formula
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
        _ = Debug.log "state" model
        svgBase extra =
            div [ -- class "resizable-svg-container" ]
                ]
                [ svg [ width "800"
                      , height "600"
                      , viewBox "-40 -30 80 60"
                      -- , Svg.Attributes.class "resizable-svg"
                      -- TODO: use when we don't hardcode svg
                      ]
                      (
                      [ drawPolygon model
                      , drawPolyline model
                      , drawStack model
                      ] ++ extra
                      )
                 ]
    in
    case model.progress_state of
        InProgress ->
            svgBase [ drawNextPoint <| trust <| nth model.next_point model.polygon
                    , drawCurrentCCW model
                    ]
        _ ->
            svgBase []


drawStack : Model -> Svg Msg
drawStack model =
    g []
      (
          -- TODO: move to constants
      [ path [ d "M -36 0 v 20 h 5 v -20" 
             , fill "none"
             , stroke "grey" ]
             []
      ] ++ List.indexedMap
             (\i n -> text_ [ x "-34.5"
                            , y <| fromInt (18 - 4*i)
                            , Svg.Attributes.class "stack-entry"
                            ]
                            [ text <| fromInt n ])
             model.stack
      )


polylineToEdges : Polyline -> List (Point, Point)
polylineToEdges polyline =
    List.map2 (\p q -> (p,q))
              polyline
              (trust <| List.tail polyline)


polygonToEdges : Polygon -> List (Point, Point)
polygonToEdges polygon =
    (polylineToEdges polygon)
    ++ [(trust <| last polygon,
         trust <| List.head polygon)]


-- Draw the polygon, return svg message
drawPolygon : Model -> Svg Msg
drawPolygon model = 
    let
        edge_click_handler i =
            case model.progress_state of
                NotStartedYet ->
                     [ onMouseDown (LeftClickEdge i)
                     , onMouseUp   (ReleasePoint)
                     ]
                _ -> []
        pt_dblclick_handler i =
            case model.progress_state of
                NotStartedYet ->
                     [onDoubleClick (DoubleClickPoint i)]
                _ -> []
        pt_click_handler i =
            case model.progress_state of
                NotStartedYet ->
                     [ onMouseDown (GrabPoint i)
                     , onMouseUp (ReleasePoint)
                     ]
                _ -> []
    in
    g []
      (
      List.indexedMap
            (\i ((x1_,y1_),(x2_,y2_)) ->
                    line ([ fill polygon_fill
                         , stroke polygon_stroke
                         , strokeWidth polygon_stroke_width
                         , strokeLinecap polygon_stroke_cap
                         , x1 <| fromFloat x1_, y1 <| fromFloat y1_
                         , x2 <| fromFloat x2_, y2 <| fromFloat y2_
                         ] ++ edge_click_handler i) [])
            (polygonToEdges model.polygon)
      ++
      List.indexedMap
            (\i (x,y) ->
                    circle ([ fill point_color
                            , cx <| fromFloat x
                            , cy <| fromFloat y
                            , r point_radius
                            ] ++ pt_dblclick_handler i
                              ++ pt_click_handler i)
                            [])
            model.polygon
      )
 
calcHullProgressPolyline : Model -> Polyline
calcHullProgressPolyline model =
    model.stack
    |> List.map (\n -> trust <| nth n model.polygon)


-- Draw every polyline, return svg message
drawPolyline : Model -> Svg Msg
drawPolyline model =
    polyline [ fill polyline_fill
             , stroke polyline_stroke
             , strokeWidth polyline_stroke_width
             , strokeLinecap polyline_stroke_cap
             , points (svgPointsFromList (calcHullProgressPolyline model))
             ]
             []


-- Draw next point in each step, return svg message
drawNextPoint : Point -> Svg Msg
drawNextPoint (x,y) =
    circle [ fill next_point_color
           , cx (fromFloat x)
           , cy (fromFloat y)
           , r point_radius
           ]
           []

polygonMidPoint : Polygon -> Point
polygonMidPoint polygon =
    let
        xsum = List.sum <| List.map Tuple.first polygon
        ysum = List.sum <| List.map Tuple.second polygon
        len = List.length polygon
    in
        ( xsum / Basics.toFloat len
        , ysum / Basics.toFloat len)
    

drawCurrentCCW : Model -> Svg Msg
drawCurrentCCW model =
    let
        _ = Debug.log "ccw state" model
        top = trust <| nth (trust <| last model.stack) model.polygon
        scd = trust <| nth (trust <| listPenultimate model.stack) model.polygon
        next = trust <| nth model.next_point model.polygon
        ccw_triangle = [scd, top, next]
        (ccw_x, ccw_y) = polygonMidPoint ccw_triangle
        _ = Debug.log "ccw" (ccw_x, ccw_y, ccw_triangle)
    in
    g []
      [ polygon [ fill ccw_triangle_fill
                , stroke ccw_triangle_stroke
                , strokeWidth ccw_triangle_stroke_width
                , strokeLinecap polygon_stroke_cap
                , strokeDasharray ccw_triangle_stroke_dash
                , points <| svgPointsFromList ccw_triangle
                ]
                []
      , image [ x <| fromFloat (ccw_x-ccw_wheel_radius)
              , y <| fromFloat (ccw_y-ccw_wheel_radius)
              , width <| fromFloat (2 * ccw_wheel_radius)
              , height <| fromFloat (2 * ccw_wheel_radius)
              , xlinkHref "static/ccw_wheel.svg"
              ] []
      ]

-- Mapping the list of points into svg attributes value
svgPointsFromList : List Point-> String
svgPointsFromList listPoint =
    listPoint
        |> List.map pointToString
        |> join " " 


-- Mapping point tuple into string
pointToString : Point -> String
pointToString (x, y) =
    fromFloat (Basics.toFloat(round(x * 100)) / 100.0)
    ++ ", "
    ++ fromFloat (Basics.toFloat(round(y * 100)) / 100.0)

writePointAction : String -> Point -> String
writePointAction action (x,y) =
    action ++ ": (" ++ pointToString (x,y) ++ ")"

listPenultimate : List a -> Maybe a
listPenultimate list =
    case List.reverse list of
        a::b::rest -> Just b
        _ -> Nothing

stackPop : Stack a -> (Maybe a, Stack a)
stackPop stack =
    case List.reverse stack of
        last::rest -> (Just last, List.reverse rest)
        [] -> (Nothing, [])

stackPush : Stack a -> a -> Stack a
stackPush stack item =
    stack ++ [item]

getBottomLeftMostPoint : Polygon -> Point
getBottomLeftMostPoint polygon =
    trust <| List.minimum polygon

    -- shift a polygon until it starts with its bottom-leftmost point
restartAtBottomLeftMost : Polygon -> Polygon
restartAtBottomLeftMost polygon =
    let
        min = getBottomLeftMostPoint polygon
    in
    case polygon of
        [] ->
            []
        first::rest ->
            if first == min
            then polygon
            else restartAtBottomLeftMost (rest ++ [first])


startAlgorithmState : Model -> Model
startAlgorithmState model =
    let 
        sorted_polygon = model.polygon
                        {- sortWith (\a b -> case compare ccw a b of)
                            model.polygon -}
        shifted_polygon = restartAtBottomLeftMost sorted_polygon
    in
    { model | polygon = shifted_polygon
            , next_point = 2
            , stack = [0,1]
            , progress_state = InProgress
            , progress_log = model.progress_log ++ [started_desc]
    }

progressConvexHull : Model -> Model
progressConvexHull model =
    case model.progress_state of
        NotStartedYet ->
            startAlgorithmState model
        InProgress ->
            if model.next_point == 1
            then { model | progress_state = Done }
            else
                let
                    top = trust <| nth (trust <| last model.stack) model.polygon
                    scd = trust <| nth (trust <| listPenultimate model.stack) model.polygon
                    next = trust <| nth model.next_point model.polygon
                in
                if ccw scd top next < 1
                then
                    { model
                      | stack = Tuple.second <| stackPop model.stack
                      , progress_log = model.progress_log
                          ++ [ul [] [li [] [text <| writePointAction "Popped point" top] ] ]
                    }
                else
                    { model
                      | stack = stackPush model.stack model.next_point
                      , next_point = remainderBy (List.length model.polygon) (model.next_point+1)  
                      , progress_log = model.progress_log
                          ++ [ul [] [li [] [text <| writePointAction "Pushed point" next] ] ]
                    }
        Done ->
            model


-- Browser Init

main : Program () Model Msg
main =
    Browser.document
        { init = (\f -> before_start_state)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
