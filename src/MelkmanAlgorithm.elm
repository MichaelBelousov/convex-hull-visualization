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
import Svg exposing (Svg, circle, g, polygon, image,
                     path, polyline, text_, animateTransform)
import Svg.Attributes exposing (..)
import Styles exposing (..)


type alias Model =
    { polygon : Polygon
    , deque : Deque Int
    , phase : Algorithm.Phase
    , next_point : Int
    }


-- progress the algorithm by one step
stepState : Model -> Model
stepState model = 
    case model.phase of
        NotStartedYet ->
            initState model
        InProgress ->
            let
                (top_is_ccw, bot_is_ccw) = calcStepInfo model
            in
                -- NOTE: consider using case?
                if top_is_ccw && bot_is_ccw
                then
                    { model
                     | next_point = model.next_point + 1
                    }
                else if not top_is_ccw
                then
                    { model
                     | deque = 
                         model.deque
                         |> Deque.pop |> Tuple.second
                         |> Deque.push model.next_point
                    }
                else if not bot_is_ccw
                then
                    { model
                     | deque =
                         model.deque
                         |> Deque.remove |> Tuple.second
                         |> Deque.insert model.next_point
                     , next_point = model.next_point + 1
                    }
                else
                    Debug.todo "Should never reach here"
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
        next = Polygon.getAt (trust <| listCyclicGet model.next_point model.deque)
                <| model.polygon
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
                (top_is_ccw, bot_is_ccw) = calcStepInfo model
            in
                if top_is_ccw && bot_is_ccw
                then
                    writeAction <| "Ignored point " ++ String.fromInt model.next_point
                else if not top_is_ccw
                then
                    let
                        top_idx = trust <| Tuple.first <| Deque.pop model.deque
                    in
                    writeAction <| "Popped " ++ String.fromInt top_idx
                             ++ " and pushed " ++ String.fromInt model.next_point
                else if not bot_is_ccw
                then
                    let
                        bot_idx = trust <| Tuple.first <| Deque.remove model.deque
                    in
                    writeAction <| "Removed " ++ String.fromInt bot_idx
                             ++ " and inserted " ++ String.fromInt model.next_point
                else
                    Debug.todo "Should never reach here"


initEmptyState : Polygon -> Model
initEmptyState polygon =
    { polygon = polygon
    , deque = []
    , phase = NotStartedYet
    , next_point = 0
    }

drawStep : Model -> Svg msg
drawStep model =
    let
        top = Polygon.getAt (trust <| listCyclicGet -1 model.deque) model.polygon
        scd_top = Polygon.getAt (trust <| listCyclicGet -2  model.deque) model.polygon
        bot = Polygon.getAt (trust <| listCyclicGet 0 model.deque) model.polygon
        scd_bot = Polygon.getAt (trust <| listCyclicGet 1 model.deque) model.polygon
        next = Polygon.getAt (trust <| listCyclicGet model.next_point model.deque) model.polygon
        (next_x, next_y) = next
        top_ccw_triangle = [scd_top, top, next]
        bot_ccw_triangle = [next, bot, scd_bot]
        (top_ccw_x, top_ccw_y) = Polygon.midpoint top_ccw_triangle
        (bot_ccw_x, bot_ccw_y) = Polygon.midpoint bot_ccw_triangle
        (top_is_ccw, bot_is_ccw) = calcStepInfo model
        ccw_svg stroke_ pts_ =
            Svg.polygon [ fill "none"
                        , stroke stroke_
                        , strokeWidth ccw_triangle_stroke_width
                        , strokeLinecap "round"
                        , strokeDasharray ccw_triangle_stroke_dash
                        , points <| svgPointsFromList pts_
                        ] []
        top_ccw_svg = ccw_svg "pink" top_ccw_triangle
        bot_ccw_svg = ccw_svg "orange" bot_ccw_triangle
        ccw_wheel_svg (x_, y_) =
            image [ x <| String.fromFloat (x_-ccw_wheel_radius)
                  , y <| String.fromFloat (y_-ccw_wheel_radius)
                  , width <| String.fromFloat (2 * ccw_wheel_radius)
                  , height <| String.fromFloat (2 * ccw_wheel_radius)
                  , xlinkHref "static/ccw_wheel.svg"
                  , transform ("translate(0,"
                            ++ String.fromFloat (2*y_)
                            ++ ") scale(1, -1)")
                  ]
                  [ animateTransform [ attributeName "transform"
                                     , type_ "rotate"
                                     , dur "1s"
                                     , repeatCount "indefinite"
                                     , from ("0 "++String.fromFloat x_++" "++String.fromFloat y_)
                                     , to ("-360 "++String.fromFloat x_++" "++String.fromFloat y_)
                                     , additive "sum"
                                     ] []
                  ]
        next_point_svg =
            circle [ fill next_point_color
                   , cx <| String.fromFloat next_x
                   , cy <| String.fromFloat next_y
                   , r point_radius
                   ]
                   []
    in
    case (top_is_ccw, bot_is_ccw) of
        (True, True) ->
            next_point_svg
        (False, True) ->
            g []
              [ bot_ccw_svg
              , ccw_wheel_svg (bot_ccw_x, bot_ccw_y)
              , next_point_svg
              ]
        (True, False) ->
            g []
              [ top_ccw_svg
              , ccw_wheel_svg (top_ccw_x, top_ccw_y)
              , next_point_svg
              ]
        (False, False) ->
            Debug.todo "Should never reach here"


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
      [ Svg.path [ d "M -36 0 v -20"
                 , fill "none"
                 , stroke "grey" ]
                 []
      , Svg.path [ d "M -31 0 v -20"
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
