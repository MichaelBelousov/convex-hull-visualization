{-
Module containing logic and types for Naive polygon convex hull finding algorithm
-}

module MelkmanAlgorithm exposing 
    ( stepState
    , describeStep
    , Model
    , initEmptyState
    , drawStep
    , drawHull
    , drawState
    )


import Html exposing (Html, text, p, div, i)
import Polygon exposing (Polygon)
import Geometry exposing (ccwTest)
import Algorithm exposing (..)
import Html exposing (Html)
import Deque exposing (Deque)
import Utils exposing (writeAction, trust,
                       pointToString, listCyclicGet,
                       svgPointsFromList)
import Svg exposing (Svg, circle, g, polygon, image, line,
                     path, polyline, text_, animateTransform)
import Svg.Attributes exposing (..)
import Styles exposing (..)


type Part
    = ConsiderNew
    | RestoreLeft
    | RestoreRight
    | Increment


type alias Model =
    { polygon : Polygon
    , deque : Deque Int
    , phase : Algorithm.Phase
    , next_point : Int
    , part : Part
    }


-- progress the algorithm by one step
stepState : Model -> Model
stepState model = 
    case model.phase of
        NotStartedYet ->
            initState model
        InProgress ->
            let
                top = Polygon.getAt (trust <| listCyclicGet -1 model.deque)
                        <| model.polygon
                scd_top = Polygon.getAt (trust <| listCyclicGet -2  model.deque)
                            <| model.polygon
                bot = Polygon.getAt (trust <| listCyclicGet 0 model.deque)
                        <| model.polygon
                scd_bot = Polygon.getAt (trust <| listCyclicGet 1 model.deque)
                            <| model.polygon
                next = Polygon.getAt model.next_point model.polygon
                _ = Debug.log "top ccw: [scd_top, top, next]" [scd_top, top, next]
                _ = Debug.log "bot ccw: [next, bot, scd_bot]" [next, bot, scd_bot]
            in
            case model.part of
                ConsiderNew ->
                    if ccwTest scd_top top next == 1
                    && ccwTest bot scd_bot next == 1
                    then
                        { model | next_point = model.next_point + 1 }
                    else
                        { model | part = RestoreLeft }
                RestoreLeft ->
                    if ccwTest scd_top top next /= 1
                    then
                        { model
                         | deque = 
                             model.deque
                             |> Deque.pop |> Tuple.second
                             |> Deque.push model.next_point
                        }
                    else
                        { model | part = RestoreRight }
                RestoreRight ->
                    if ccwTest next bot scd_bot /= 1
                    then
                        { model
                         | deque =
                             model.deque
                             |> Deque.remove |> Tuple.second
                             |> Deque.insert model.next_point
                        }
                    else
                        { model | part = Increment }
                Increment ->
                    let
                        next_next_point = model.next_point + 1
                    in
                        if next_next_point >= List.length model.polygon
                        then
                            { model | phase = Done }
                        else
                            { model
                             | part = ConsiderNew
                             , next_point = next_next_point
                            }
        Done ->
            model


-- initialization step of the algorithm
initState : Model -> Model
initState model =
    let
        fst = Polygon.getAt 0 model.polygon
        scd = Polygon.getAt 1 model.polygon
        thrd = Polygon.getAt 2 model.polygon
    in
        { model 
          | next_point = 3
          , deque = if ccwTest fst scd thrd == 1
                then [2, 0, 1, 2]
                else [2, 1, 0, 2]
          , phase = InProgress
        }


started_desc : Html msg
started_desc =
    div []
        [ p []
            [ text ("The Melkman algorithm is the latest (1987) and considered the best "
                 ++ "linear-time algorithm for finding a polygon's convex hull. "
                 ++ "This time, we don't need to shift our polygon until we find a "
                 ++ "counter-clockwise oriented triple, and we won't even check if "
                 ++ "our polygon was given in counter-clockwise order exactly. ")
            ]
        , p []
            [ text ("Instead of a stack, we'll use a deque, which is like a stack that "
                 ++ "you can pop and push on both ends. Pushing and popping the bottom are "
                 ++ "referred to as ")
            , i [] [ text "inserting "]
            , text  "and "
            , i [] [ text "removing. "]
            ]
        , p []
            [ text ("We start by grabbing the first three element of our polygon from "
                 ++ "anywhere, whatever order we had them in at first is convenient. "
                 ++ "Then we make a triangle, a convex hull of the already considered points "
                 ++ "in counter clockwise order. If they're already CCW, then that's just ")
            , i [] [ text "[third, first, second, third] "]
            , text  "otherwise we do "
            , i [] [ text "[third, second, first, third]"]
            , text  ". This algorithm is "
            , i [] [ text "incremental "]
            , text ("meaning, it will have already found the convex hull of whatever points "
                 ++ "have already been considered")
            ]
        ]


calcStepInfo : Model -> (Bool, Bool)
calcStepInfo model =
    let
        top = Polygon.getAt (trust <| listCyclicGet -1 model.deque)
                <| model.polygon
        scd_top = Polygon.getAt (trust <| listCyclicGet -2  model.deque)
                    <| model.polygon
        bot = Polygon.getAt (trust <| listCyclicGet 0 model.deque)
                <| model.polygon
        scd_bot = Polygon.getAt (trust <| listCyclicGet 1 model.deque)
                    <| model.polygon
        next = Polygon.getAt model.next_point model.polygon
        top_is_ccw = ccwTest scd_top top next == 1
        bot_is_ccw = ccwTest next bot scd_bot == 1
    in
        (top_is_ccw, bot_is_ccw)


-- describe what the algorithm would do this step
describeStep : Model -> Html m
describeStep model =
    case model.phase of
        NotStartedYet ->
            started_desc
        _ ->
            let
                top_idx = trust <| listCyclicGet -1 model.deque
                top = Polygon.getAt top_idx model.polygon
                scd_top = Polygon.getAt (trust <| listCyclicGet -2  model.deque) model.polygon
                bot_idx = trust <| listCyclicGet 0 model.deque
                bot = Polygon.getAt bot_idx model.polygon
                scd_bot = Polygon.getAt (trust <| listCyclicGet 1 model.deque) model.polygon
                next = Polygon.getAt model.next_point model.polygon
            in
            case model.part of
                ConsiderNew ->
                    if ccwTest scd_top top next == 1
                    && ccwTest bot scd_bot next == 1
                    then
                        writeAction <| "Ignored point " ++ String.fromInt model.next_point
                    else
                        div [] []
                RestoreLeft ->
                    if ccwTest scd_top top next /= 1
                    then
                        writeAction <| "Popped " ++ String.fromInt top_idx
                               ++ " and pushed " ++ String.fromInt model.next_point
                    else
                        div [] []
                RestoreRight ->
                    if ccwTest next bot scd_bot /= 1
                    then
                        writeAction <| "Removed " ++ String.fromInt top_idx
                              ++ " and inserted " ++ String.fromInt model.next_point
                    else
                        div [] []
                Increment ->
                    let
                        next_next_point = model.next_point + 1
                    in
                        if next_next_point >= List.length model.polygon
                        then writeAction "Out of points, done"
                        else writeAction "Convexity restored, advance a point"


initEmptyState : Polygon -> Model
initEmptyState polygon =
    { polygon = polygon
    , deque = []
    , phase = NotStartedYet
    , next_point = 0
    , part = ConsiderNew
    }


ccwWheelSvg (pos_x, pos_y) bad_ccw =
    g []
      (
      [ image [ x <| String.fromFloat (pos_x-ccw_wheel_radius)
              , y <| String.fromFloat (pos_y-ccw_wheel_radius)
              , width <| String.fromFloat (2 * ccw_wheel_radius)
              , height <| String.fromFloat (2 * ccw_wheel_radius)
              , xlinkHref "static/ccw_wheel.svg"
              , transform ("translate(0,"
                        ++ String.fromFloat (2*pos_y)
                        ++ ") scale(1, -1)")
              ]
              [ animateTransform [ attributeName "transform"
                                 , type_ "rotate"
                                 , dur "1s"
                                 , repeatCount "indefinite"
                                 , from ("0 "++String.fromFloat pos_x++" "++String.fromFloat pos_y)
                                 , to ("-360 "++String.fromFloat pos_x++" "++String.fromFloat pos_y)
                                 , additive "sum"
                                 ] []
              ]
      ] ++ (
          if bad_ccw
          then [ line [ x1 <| String.fromFloat <| pos_x-ccw_wheel_radius
                      , y1 <| String.fromFloat <| pos_y-ccw_wheel_radius
                      , x2 <| String.fromFloat <| pos_x+ccw_wheel_radius
                      , y2 <| String.fromFloat <| pos_y+ccw_wheel_radius
                      , stroke "red"
                      ] []
               ]
          else []
          )
      )


drawStep : Model -> Svg msg
drawStep model =
    case model.phase of
        NotStartedYet ->
            g [] []
        _ ->
            let
                top = Polygon.getAt (trust <| listCyclicGet -1 model.deque) model.polygon
                scd_top = Polygon.getAt (trust <| listCyclicGet -2  model.deque) model.polygon
                bot = Polygon.getAt (trust <| listCyclicGet 0 model.deque) model.polygon
                scd_bot = Polygon.getAt (trust <| listCyclicGet 1 model.deque) model.polygon
                next = Polygon.getAt model.next_point model.polygon
                (next_x, next_y) = next
                top_ccw_triangle = [scd_top, top, next]
                bot_ccw_triangle = [next, bot, scd_bot]
                (top_ccw_x, top_ccw_y) = Polygon.midpoint top_ccw_triangle
                (bot_ccw_x, bot_ccw_y) = Polygon.midpoint bot_ccw_triangle
                (top_is_ccw, bot_is_ccw) = calcStepInfo model
                ccwSvg color pts_ =
                    Svg.polygon [ fill color
                                , fillOpacity "0.3"
                                , stroke color
                                , strokeWidth ccw_triangle_stroke_width
                                , strokeLinecap "round"
                                , strokeDasharray ccw_triangle_stroke_dash
                                , points <| svgPointsFromList pts_
                                ] []
                top_ccw_svg = ccwSvg "pink" top_ccw_triangle
                bot_ccw_svg = ccwSvg "orange" bot_ccw_triangle
                next_point_svg =
                    circle [ fill next_point_color
                           , cx <| String.fromFloat next_x
                           , cy <| String.fromFloat next_y
                           , r point_radius
                           ]
                           []
            in
                g []
                  [ top_ccw_svg
                  , ccwWheelSvg (top_ccw_x, top_ccw_y) top_is_ccw
                  , bot_ccw_svg
                  , ccwWheelSvg (bot_ccw_x, bot_ccw_y) bot_is_ccw
                  , next_point_svg
                  ]


drawHull : Model -> Svg msg
drawHull model =
    let
        hull =
            model.deque
            |> List.map (\n -> Polygon.getAt n model.polygon)
    in
    polyline (Styles.hull
             [ points
                <| svgPointsFromList
                <| hull
             ]
             ) []


drawState : Model -> Svg msg
drawState model =
    let
        len = List.length model.deque
        deque_center_y = 10 - 2*len
    in
    g []
      (
          -- TODO: move to constants
      [ Svg.path [ d "M -36 -10 v 30"
                 , fill "none"
                 , stroke "grey" ]
                 []
      , Svg.path [ d "M -31 -10 v 30"
                 , fill "none"
                 , stroke "grey" ]
                 []
      ] ++ List.indexedMap
             (\i n -> text_ [ x "-33.5"
                            , y <| String.fromInt (deque_center_y - 4*i)
                            , class "stack-entry"
                            , cartesian_flip
                            ]
                            [ text <| String.fromInt n ])
             model.deque
      )
