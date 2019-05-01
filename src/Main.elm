module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, Attribute, div, button, text, a,
                      table, tr, td, p, i, b, ul, ol, li)
import Html.Events exposing (onClick, onDoubleClick, onMouseUp, onMouseDown)
import Html.Attributes exposing (..)
import List
import List.Extra exposing (getAt, last, splitAt)
import Tuple
import Debug
import Json.Decode as Decode
import Json.Encode as Encode
import String exposing (..)
import Svg exposing (Svg, svg, circle, polyline, polygon,
                     line, g, path, image, text_, animateTransform)
import Svg.Attributes exposing (height, width, viewBox, xlinkHref, id,
                                fill, stroke, strokeWidth, strokeLinecap,
                                strokeDasharray, cx, cy, r, points, d, x, y,
                                x1, y1, x2, y2, transform, attributeName,
                                type_, dur, repeatCount, from, to, additive,
                                dx, dy)
import SvgPorts exposing (mouseToSvgCoords, decodeSvgPoint)
import ScrollPorts exposing (scrollToBottom)
import Math.Vector2 as Vec2D exposing (Vec2, vec2)

-- Browser Model

type Msg
    = StepAlgorithm
    | DoubleClickPoint Int
    | LeftClickEdge Int
    | GrabPoint Int
    | ReleasePoint
    | MouseMoved Encode.Value
    | DoNothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        grabbed_moved =
            case model.grabbed of
                Just grabbed ->
                     { model
                       | polygon = List.indexedMap
                                       (\i p -> if i == grabbed
                                                then model.mouse_in_svg
                                                else p)
                                       model.polygon
                     }
                Nothing ->
                    model
        andScroll model_ = ( model_, scrollToBottom "progress-log" )
        nocmd model_ = ( model_, Cmd.none )
    in
    case msg of
        StepAlgorithm ->
            andScroll <| case model.progress_state of
                Done ->
                    { before_start_state | polygon = model.polygon }
                _ ->
                    progressConvexHull grabbed_moved
        DoubleClickPoint point_idx ->
            deletePoint model point_idx
            |> nocmd
        LeftClickEdge edge_idx ->
            let
                insert_done = insertPoint grabbed_moved edge_idx
            in
                { insert_done | grabbed = Just (edge_idx+1) }
                |> nocmd
        GrabPoint point_idx ->
            nocmd <| { grabbed_moved | grabbed = Just point_idx }
        ReleasePoint ->
            nocmd <| { grabbed_moved | grabbed = Nothing }
        MouseMoved received ->
            nocmd <|
            case Decode.decodeValue decodeSvgPoint received of
                Ok {x, y} ->
                    { grabbed_moved | mouse_in_svg = (x,y) }
                Err _ ->
                    Debug.todo "bad value sent over svgCoords port sub"
        DoNothing ->
            nocmd <| model

keyDecoder : Decode.Decoder Msg
keyDecoder = 
    Decode.map whichKey (Decode.field "key" Decode.string)

whichKey : String -> Msg
whichKey string =
    case string of
        "Enter" ->
            StepAlgorithm
        " " ->
            StepAlgorithm
        _ -> 
            DoNothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ mouseToSvgCoords MouseMoved
        , onKeyDown keyDecoder
        ]

view : Model -> Browser.Document Msg
view model =
    let
        btn_label = case model.progress_state of
            NotStartedYet ->
                "start!"
            InProgress ->
                "next step"
            Done ->
                "restart"
    in
    { title = app_title
    , body = [
    div []
        [ div [] [ table [ style  "width" "100%"
                         , style "table-layout" "fixed"
                         ]
                         [ tr []
                              [ td [ class "visualization" ]
                                   [ div [] [ drawConvexHullAlgorithmsState model ]

                                   ]
                              , td [ class "description" ]
                                   [ div [ class "progress-log"
                                         , id progress_log_id
                                         ]
                                         model.progress_log
                                    , div [ class "next-btn-container" ]
                                         [ button [ onClick StepAlgorithm]
                                                  [ text btn_label ]
                                         ]
                                   ]

                              ]
                         ]
                 ]
        , div [ class "footer" ]
              [ a [ href "about.html" ] [ text "about" ] ]
        ]
    ]}

type alias Model =
    { polygon : Polygon
    , stack : Stack Int
    , progress_state : ModelState
    , next_point : Int
    , grabbed : Maybe Int
    , progress_log : List (Html Msg)
    , mouse_in_svg : Point
    }

type ModelState
    = NotStartedYet
    | InProgress
    | Done

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
cartesian_area = transform "scale(1, -1)"
cartesian_flip= transform "scale(1, -1)"
label_x_shift = dx "2.5"
label_y_shift = dy "2.5"
label_offset = 0.2
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
origin_fill = "none"
origin_stroke = "black"
origin_stroke_width = fromFloat 0.3

progress_log_id = "progress-log"

-- NOTE: generate z-order constants from a priority list?

-- UI Strings

intro : Html Msg
intro = div
        []
        [ p []
            [ text "Welcome. Together, we're going to find the "
            , i [] [ text "convex hull " ]
            , text "of this "
            , i [] [ text "simple polygon " ]
            , text ("on the left. A polygon is just a sequence of points, "
                 ++ "with straight edges between each sequential pair, such "
                 ++ "as a square, or arrow shape. You've probably heard of "
                 ++ "polygons before. A convex hull is what you get when you "
                 ++ "put a rubber band around one.  it's like the same polygon "
                 ++ "but you've removed all the dents.")
            ]
        , p []
            [ text "You can ignore the grey "
            , i [] [ text "stack " ]
            , text ("on the left, we'll use it soon, and that plus sign "
                 ++ "is just the origin for your reference. "
                 ++ "For now, feel free to build your own polygon by "
                 ++ "interacting with the available tools. "
                 ++ "When you're done, hit the \"Start!\" button.")
            ]
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
                 ++ "Our simple polygon is already sorted in counter-clockwise (CCW) order "
                 ++ "(if it weren't we'd just reverse it), so we'll just find the "
                 ++ "bottom-leftmost point and shift the polygon list to start at that point.")
            ]
        , p []
            [ text ("To start, we put the first two points of our polygon in a stack, "
                 ++ "and we start considering the remaining points in order. The point "
                 ++ "we're considering is in yellow, and the dashed yellow triangle "
                 ++ "is a CCW test between the top two members of the stack, and that "
                 ++ "point of consideration. Note the black spinny arrow that should "
                 ++ "helpfully illustrate whether the triangle's points are in CCW order.")
            ]
        ]

-- Utilities

makeCube : Float -> Polygon
makeCube half_sz =
    [ (-half_sz,  -half_sz)
    , (half_sz, -half_sz)
    , (half_sz, half_sz)
    , (-half_sz, half_sz) ]

-- Interactions

deletePoint : Model -> Int -> Model
deletePoint model point_idx =
    let
        point = trust <| getAt point_idx model.polygon
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
before_start_state : Model
before_start_state =
    { polygon = init_polygon
    , stack = []
    , progress_state = NotStartedYet
    , next_point = -1
    , grabbed = Nothing
    , progress_log = [intro]
    , mouse_in_svg = (0,0)
    }

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
        svgBase extra =
            div [ class "resizable-svg-container" ]
                [ svg [ viewBox "-40 -12 80 30"
                      , Svg.Attributes.class "resizable-svg"
                      , cartesian_area
                      ]
                      (
                      [ drawOrigin
                      , drawStack model
                      , drawPolygon model
                      , drawPolyline model
                      ] ++ extra ++
                      [ drawVertsIndex model
                      ]
                      )
                 ]
    in
    case model.progress_state of
        InProgress ->
            svgBase [ drawNextPoint <| trust <| getAt model.next_point model.polygon
                    , drawCurrentCCW model
                    ]
        _ ->
            svgBase []

drawOrigin : Svg Msg
drawOrigin =
    path [ d "M 0 0 h 5 h -10 h 5 v 5 v -10"
         , fill origin_fill
         , stroke origin_stroke
         , strokeWidth origin_stroke_width
         ]
         []


drawStack : Model -> Svg Msg
drawStack model =
    g []
      (
          -- TODO: move to constants
      [ path [ d "M -36 0 v -20 h 5 v 20"
             , fill "none"
             , stroke "grey" ]
             []
      ] ++ List.indexedMap
             (\i n -> text_ [ x "-33.5"
                            , y <| fromInt (18 - 4*i)
                            , Svg.Attributes.class "stack-entry"
                            , cartesian_flip
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
    case model.progress_state of
        -- attach  polygon editing handlers if algorithm not started yet
        NotStartedYet ->
            let
             edge_click_handlers i = [
                                     ]
            in
            g []
              (  drawPolygonEdges model.polygon (\i -> [ onMouseDown (LeftClickEdge i)
                                                       , onMouseUp   (ReleasePoint)
                                                       ])
              ++ drawPolygonVerts model.polygon (\i -> [ onDoubleClick (DoubleClickPoint i)
                                                       , onMouseDown   (GrabPoint i)
                                                       , onMouseUp     (ReleasePoint)
                                                       ])
              )
        _ ->
            g []
              (  drawPolygonEdges model.polygon (\i->[])
              ++ drawPolygonVerts model.polygon (\i->[])
              )

drawPolygonEdges : Polygon -> (Int -> List (Attribute m)) -> List (Svg m)
drawPolygonEdges polygon interactions =
    List.indexedMap
        (\i ((x1_,y1_),(x2_,y2_)) ->
                line ([ fill polygon_fill
                     , stroke polygon_stroke
                     , strokeWidth polygon_stroke_width
                     , strokeLinecap polygon_stroke_cap
                     , x1 <| fromFloat x1_, y1 <| fromFloat y1_
                     , x2 <| fromFloat x2_, y2 <| fromFloat y2_
                     ] ++ interactions i) [])
        (polygonToEdges polygon)

drawPolygonVerts : Polygon -> (Int -> List (Attribute m)) -> List (Svg m)
drawPolygonVerts polygon interactions =
    List.indexedMap
        (\i (x,y) ->
                circle (
                       [ fill point_color
                       , cx <| fromFloat x
                       , cy <| fromFloat y
                       , r point_radius
                       ] ++ interactions i
                       )
                       []
                       )
        polygon

calcHullProgressPolyline : Model -> Polyline
calcHullProgressPolyline model =
    (case model.progress_state of
        Done ->
            stackPush model.stack 0
        _ ->
            model.stack)
    |> List.map (\n -> trust <| getAt n model.polygon)


-- Draw every polyline, return svg message
drawPolyline : Model -> Svg Msg
drawPolyline model =
    polyline [ fill polyline_fill
             , stroke polyline_stroke
             , strokeWidth polyline_stroke_width
             , strokeLinecap polyline_stroke_cap
             , points <| svgPointsFromList <| calcHullProgressPolyline model
             ]
             []


-- Draw the next consideration point in the naive algorithm
drawNextPoint : Point -> Svg Msg
drawNextPoint (x,y) =
    circle [ fill next_point_color
           , cx <| fromFloat x
           , cy <| fromFloat y
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
        top = trust <| getAt (trust <| last model.stack) model.polygon
        scd = trust <| getAt (trust <| listPenultimate model.stack) model.polygon
        next = trust <| getAt model.next_point model.polygon
        ccw_triangle = [scd, top, next]
        (ccw_x, ccw_y) = polygonMidPoint ccw_triangle
    in
    g []
      [ polygon [ fill ccw_triangle_fill
                , stroke ccw_triangle_stroke
                , strokeWidth ccw_triangle_stroke_width
                , strokeLinecap polygon_stroke_cap
                , strokeDasharray ccw_triangle_stroke_dash
                , points <| svgPointsFromList ccw_triangle
                ] []
        -- flip with corrective translation
      , image [ x <| fromFloat (ccw_x-ccw_wheel_radius)
              , y <| fromFloat (ccw_y-ccw_wheel_radius)
              , width <| fromFloat (2 * ccw_wheel_radius)
              , height <| fromFloat (2 * ccw_wheel_radius)
              , xlinkHref "static/ccw_wheel.svg"
              , transform ("translate(0,"
                        ++ fromFloat (2*ccw_y)
                        ++ ") scale(1, -1)")
              ]
              [ animateTransform [ attributeName "transform"
                                 , type_ "rotate"
                                 , dur "1s"
                                 , repeatCount "indefinite"
                                 , from ("0 "++fromFloat ccw_x++" "++fromFloat ccw_y)
                                 , to ("-360 "++fromFloat ccw_x++" "++fromFloat ccw_y)
                                 , additive "sum"
                                 ] []
              ]
      ]

polygonGetRelative : Int -> Polygon -> Point
polygonGetRelative n polygon =
    let
        len = List.length polygon
        rel_idx = if n < 0 then len - (abs n)
                           else n
        idx = remainderBy len rel_idx
    in
    trust <| getAt idx polygon

drawVertsIndex : Model -> Svg Msg
drawVertsIndex model =
    g [] <| List.indexedMap
            (\i (vx,vy) ->
                let
                    (prev_x, prev_y) = polygonGetRelative (i-1) model.polygon
                    prev = Vec2D.vec2 prev_x prev_y
                    (curr_x, curr_y) = polygonGetRelative i model.polygon
                    curr = Vec2D.vec2 curr_x curr_y
                    (next_x, next_y) = polygonGetRelative (i+1) model.polygon
                    next = Vec2D.vec2 next_x next_y
                    -- centered around origin
                    cprev = Vec2D.sub prev curr
                    cnext = Vec2D.sub next curr
                    clabel = Vec2D.scale (-label_offset)
                            <| Vec2D.scale 0.5 <| Vec2D.add cprev cnext
                    label = Vec2D.add clabel curr
                    (label_x, label_y) = (Vec2D.getX label, Vec2D.getY label)
                in
                    text_ [ x <| fromFloat label_x
                          , y <| fromFloat (-label_y)
                          , Svg.Attributes.class "point-label"
                          , cartesian_flip
                          ]
                          [ text <| fromInt i ]
            )
            model.polygon


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

writePointAction : String -> Point -> Int -> String
writePointAction action (x,y) index =
    action ++ " point: " ++ fromInt index
    ++ " at (" ++ pointToString (x,y) ++ ")"

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


svgToCartesian : Polygon -> Polygon
svgToCartesian pts =
    List.map (\(x,y)->(x,-y)) pts


startAlgorithmState : Model -> Model
startAlgorithmState model =
    let
        shifted_polygon = restartAtBottomLeftMost model.polygon
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
            let
                top_idx = trust <| last model.stack
                top = trust <| getAt top_idx model.polygon
                scd = trust <| getAt (trust <| listPenultimate model.stack) model.polygon
                next = trust <| getAt model.next_point model.polygon
                is_not_ccw = ccw scd top next < 1
                next_stack = case (is_not_ccw, model.next_point) of
                    (True, _) ->
                        Tuple.second <| stackPop model.stack
                    (False, 0) ->
                        model.stack -- don't push the first point again
                    (False, _) ->
                        stackPush model.stack model.next_point
                next_log = model.progress_log
                        ++ (if is_not_ccw
                            then [ul [] [li [] [text <| writePointAction
                                                            "Popped"
                                                            top
                                                            top_idx]]]
                            else [ul [] [li [] [text <| writePointAction
                                                            "Pushed"
                                                            next
                                                            model.next_point]]])
            in
            case model.next_point of
                0 ->
                    { model
                      | progress_state = Done
                      , stack = next_stack
                      , progress_log = next_log
                    }
                _ ->
                    if is_not_ccw
                    then { model
                           | stack = next_stack
                           , progress_log = next_log
                         }
                    else { model
                           | stack = next_stack
                           , next_point = remainderBy (List.length model.polygon)
                                                      (model.next_point+1)
                           , progress_log = next_log
                         }
        Done ->
            model


-- Browser Init

main : Program () Model Msg
main =
    Browser.document
        { init = (\f -> ( before_start_state, Cmd.none))
        , view = view
        , update = (\msg model -> update msg model)
        , subscriptions = subscriptions
        }
