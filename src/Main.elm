module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import Html exposing (Html, Attribute, div, button, text, a,
                      table, tr, td, p, i, b, ul, ol, li)
import Html.Events exposing (onClick, onDoubleClick, onMouseUp, onMouseDown)
import Html.Attributes exposing (..)
-- import List
import List.Extra exposing (getAt, last, splitAt)
-- import Tuple
-- import Debug
import Json.Decode as Decode
import Json.Encode as Encode
import String
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
import Geometry exposing (Point, ccwTest)
import Polygon exposing (Polygon)
import Polyline exposing (Polyline)
import Utils exposing (..)
import Styles
import Algorithm exposing (..)
import NaiveAlgorithm

-- Browser Model

type Msg
    = StepAlgorithm
    | DoubleClickPoint Int
    | LeftClickEdge Int
    | GrabPoint Int
    | ReleasePoint
    | MouseMoved Encode.Value
    | DoNothing

-- update the grabbed point
updateGrab : Model -> Model
updateGrab model =
    case model.grabbed of
        Just grabbed ->
            let
                algo_state = model.algo_state
                new_algo_state = 
                    { algo_state
                     | polygon = List.indexedMap
                                     (\i p -> if i == grabbed
                                             then model.mouse_in_svg
                                             else p)
                                     model.algo_state.polygon
                    }
            in
                { model | algo_state = new_algo_state }
        Nothing ->
            model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        grabbed_moved = updateGrab model
        andScroll model_ = ( model_, scrollToBottom "progress-log" )
        nocmd model_ = ( model_, Cmd.none )
    in
    case msg of
        StepAlgorithm ->
            andScroll <| case model.algo_state.phase of
                Done ->
                    { model
                     | algo_state = NaiveAlgorithm.initEmptyState model.algo_state.polygon
                    }
                _ ->
                    { model
                     | algo_state = NaiveAlgorithm.stepState grabbed_moved.algo_state
                     , progress_log = model.progress_log
                        ++ [NaiveAlgorithm.describeStep grabbed_moved.algo_state]
                    }
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
        , onKeyUp keyDecoder
        ]

view : Model -> Browser.Document Msg
view model =
    { title = app_title
    , body =
        [ div []
              [ div []
                    [ table [ style  "width" "100%"
                            , style "table-layout" "fixed"
                            ]
                            [ tr []
                                 [ viewVisualization model
                                 , viewNarration model
                                 ]
                            ]
                     ]
              , div [ class "footer" ]
                    [ a [ href "about.html" ] [ text "about" ] ]
              ]
        ]
    }


viewVisualization : Model -> Html Msg
viewVisualization model =
    td [ class "visualization" ]
       [ div [] [ drawConvexHullAlgorithmsState model ] ]


viewNarration : Model -> Html Msg
viewNarration model =
    let
        btn_label = case model.algo_state.phase of
            NotStartedYet ->
                "start!"
            InProgress ->
                "next step"
            Done ->
                "restart"
    in
        td [ class "narration" ]
           [ div [ class "progress-log"
                 , id progress_log_id
                 ]
                 model.progress_log
            , div [ class "next-btn-container" ]
                  [ button [ onClick StepAlgorithm ]
                           [ text btn_label ]
                  ]
           ]


type alias Model =
    { grabbed : Maybe Int
    , progress_log : List (Html Msg)
    , mouse_in_svg : Point
    , algo_state : NaiveAlgorithm.Model
    }


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
point_radius = String.fromFloat 2
next_point_color = "yellow"
ccw_triangle_fill = "none"
ccw_triangle_stroke = "yellow"
ccw_triangle_stroke_width = String.fromFloat 0.7
ccw_triangle_stroke_dash = "3,2"
ccw_wheel_radius = 5
ccw_wheel_id = "ccw_wheel"
origin_fill = "none"
origin_stroke = "black"
origin_stroke_width = String.fromFloat 0.3

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
                 ++ "When you're done, hit the 'Start!' button.")
            ]
        , ul []
             [ li [] [ text "Click and drag on points to move them around"]
             , li [] [ text "Double click on a point to delete it"]
             , li [] [ text "Click and drag on edges to add points"]
             ]
        , p []
            [ text ("If you haven't looked it up already, a simple polygon "
                 ++ "doesn't have any of its edges intersecting or overlapping. "
                 ++ "If you decide to do that, you will break the algorithms, "
                 ++ "but it's worth experimenting with anyway.")
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
        polygon = model.algo_state.polygon
        point = trust <| getAt point_idx polygon
        algo_state = model.algo_state
        new_algo_state = { algo_state | polygon = List.filter (\p -> p /= point) polygon }
    in
        if List.length polygon > 3
        then { model | algo_state = new_algo_state }
        else model


insertPoint : Model -> Int -> Model
insertPoint model edge_idx =
    let
        polygon = model.algo_state.polygon
        (front, back) = splitAt (edge_idx+1) polygon
        mdpt = case (last front, List.head back) of
                (Just x, Just y) ->
                    Polygon.midpoint [x, y]
                (Just x, Nothing) ->
                    Polygon.midpoint [ trust <| List.head front
                                     , trust <| last front
                                     ]
                (Nothing, Just y) ->
                    Polygon.midpoint [ trust <| List.head back
                                     , trust <| last back
                                     ]
                _ -> Debug.todo "bad polygon"
        new_polygon = front ++ [mdpt] ++ back
        algo_state = model.algo_state
        new_algo_state = { algo_state | polygon = new_polygon }
    in
    { model | algo_state = new_algo_state }

-- initial page state

    -- state when the app starts
before_start_state : Model
before_start_state =
    { grabbed = Nothing
    , progress_log = [intro]
    , mouse_in_svg = (0,0)
    , algo_state = NaiveAlgorithm.initEmptyState init_polygon
    }


-- trust that a Maybe is fulfilled
trust : Maybe a -> a
trust x =
    case x of
        Just y -> y
        Nothing -> Debug.todo "trust got Nothing"


drawConvexHullAlgorithmsState : Model -> Html Msg
drawConvexHullAlgorithmsState model =
    let
        polygon = model.algo_state.polygon
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
    case model.algo_state.phase of
        InProgress ->
            svgBase [ drawNextPoint <| trust <| getAt model.algo_state.next_point polygon
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
                            , y <| String.fromInt (18 - 4*i)
                            , Svg.Attributes.class "stack-entry"
                            , cartesian_flip
                            ]
                            [ text <| String.fromInt n ])
             model.algo_state.stack
      )



-- Draw the polygon, return svg message
drawPolygon : Model -> Svg Msg
drawPolygon model =
    let
        polygon = model.algo_state.polygon
    in
    case model.algo_state.phase of
        -- attach  polygon editing handlers if algorithm not started yet
        NotStartedYet ->
            g []
              (  drawPolygonEdges polygon (\i -> [ onMouseDown (LeftClickEdge i)
                                                 , onMouseUp   (ReleasePoint)
                                                 ])
              ++ drawPolygonVerts polygon (\i -> [ onDoubleClick (DoubleClickPoint i)
                                                 , onMouseDown   (GrabPoint i)
                                                 , onMouseUp     (ReleasePoint)
                                                 ])
              )
        _ ->
            g []
              (  drawPolygonEdges polygon (\i->[])
              ++ drawPolygonVerts polygon (\i->[])
              )

drawPolygonEdges : Polygon -> (Int -> List (Attribute m)) -> List (Svg m)
drawPolygonEdges polygon interactions =
    List.indexedMap
        (\i ((x1_,y1_),(x2_,y2_)) ->
                line (Styles.polygon 
                     ([ x1 <| String.fromFloat x1_, y1 <| String.fromFloat y1_
                      , x2 <| String.fromFloat x2_, y2 <| String.fromFloat y2_
                      ] ++ interactions i)
                     )
                     [])
        (Polygon.getEdges polygon)

drawPolygonVerts : Polygon -> (Int -> List (Attribute m)) -> List (Svg m)
drawPolygonVerts polygon interactions =
    List.indexedMap
        (\i (x,y) ->
                circle (
                       [ fill point_color
                       , cx <| String.fromFloat x
                       , cy <| String.fromFloat y
                       , r point_radius
                       ] ++ interactions i
                       )
                       []
                       )
        polygon

calcHullProgressPolyline : Model -> Polyline
calcHullProgressPolyline model =
    model.algo_state.stack
    |> List.map (\n -> Polygon.getAt n model.algo_state.polygon)


-- Draw every polyline, return svg message
drawPolyline : Model -> Svg Msg
drawPolyline model =
    polyline (Styles.hull
             [ points <| svgPointsFromList <| calcHullProgressPolyline model
             ]
             )
             []


-- Draw the next consideration point in the naive algorithm
drawNextPoint : Point -> Svg Msg
drawNextPoint (x,y) =
    circle [ fill next_point_color
           , cx <| String.fromFloat x
           , cy <| String.fromFloat y
           , r point_radius
           ]
           []


drawCurrentCCW : Model -> Svg Msg
drawCurrentCCW model =
    let
        top = trust <| getAt (trust <| last model.algo_state.stack) model.algo_state.polygon
        scd = trust <| getAt (trust <| listCyclicGet -2 model.algo_state.stack) model.algo_state.polygon
        next = trust <| getAt model.algo_state.next_point model.algo_state.polygon
        ccw_triangle = [scd, top, next]
        (ccw_x, ccw_y) = Polygon.midpoint ccw_triangle
    in
    g []
      [ polygon [ fill ccw_triangle_fill
                , stroke ccw_triangle_stroke
                , strokeWidth ccw_triangle_stroke_width
                , strokeLinecap "round"
                , strokeDasharray ccw_triangle_stroke_dash
                , points <| svgPointsFromList ccw_triangle
                ] []
        -- flip with corrective translation
      , image [ x <| String.fromFloat (ccw_x-ccw_wheel_radius)
              , y <| String.fromFloat (ccw_y-ccw_wheel_radius)
              , width <| String.fromFloat (2 * ccw_wheel_radius)
              , height <| String.fromFloat (2 * ccw_wheel_radius)
              , xlinkHref "static/ccw_wheel.svg"
              , transform ("translate(0,"
                        ++ String.fromFloat (2*ccw_y)
                        ++ ") scale(1, -1)")
              ]
              [ animateTransform [ attributeName "transform"
                                 , type_ "rotate"
                                 , dur "1s"
                                 , repeatCount "indefinite"
                                 , from ("0 "++String.fromFloat ccw_x++" "++String.fromFloat ccw_y)
                                 , to ("-360 "++String.fromFloat ccw_x++" "++String.fromFloat ccw_y)
                                 , additive "sum"
                                 ] []
              ]
      ]


drawVertsIndex : Model -> Svg Msg
drawVertsIndex model =
    g [] <| List.indexedMap
            (\i (vx,vy) ->
                let
                    polygon = model.algo_state.polygon
                    (prev_x, prev_y) = Polygon.getAt (i-1) polygon
                    prev = Vec2D.vec2 prev_x prev_y
                    (curr_x, curr_y) = Polygon.getAt i polygon
                    curr = Vec2D.vec2 curr_x curr_y
                    (next_x, next_y) = Polygon.getAt (i+1) polygon
                    next = Vec2D.vec2 next_x next_y
                    -- centered around origin
                    cprev = Vec2D.sub prev curr
                    cnext = Vec2D.sub next curr
                    clabel = Vec2D.scale (-label_offset) -- NOTE: could clamp by normalized + offset
                            <| Vec2D.scale 0.5 <| Vec2D.add cprev cnext
                    label = Vec2D.add clabel curr
                    (label_x, label_y) = (Vec2D.getX label, Vec2D.getY label)
                in
                    text_ [ x <| String.fromFloat label_x
                          , y <| String.fromFloat (-label_y)
                          , Svg.Attributes.class "point-label"
                          , cartesian_flip
                          ]
                          [ text <| String.fromInt i ]
            )
            model.algo_state.polygon


-- Mapping the list of points into svg attributes value
svgPointsFromList : List Point-> String
svgPointsFromList listPoint =
    listPoint
        |> List.map pointToString
        |> String.join " "


-- Browser Init

main : Program () Model Msg
main =
    Browser.document
        { init = (\f -> ( before_start_state, Cmd.none))
        , view = view
        , update = (\msg model -> update msg model)
        , subscriptions = subscriptions
        }
