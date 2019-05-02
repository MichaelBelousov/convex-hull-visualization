{-
Module containing logic and types for Naive polygon convex hull finding algorithm
-}

module NaiveAlgorithm exposing 
    ( stepState
    , describeStep
    , Model
    , initEmptyState
    , drawStep
    , drawHull
    , drawState
    , counter_example
    )


import Html exposing (Html, text, p, div, i)
import Polygon exposing (Polygon)
import Geometry exposing (Point, ccwTest)
import Algorithm exposing (..)
import Html exposing (Html)
import Stack exposing (Stack)
import Utils exposing (writePointAction, trust,
                       pointToString, listCyclicGet,
                       svgPointsFromList)
import Svg exposing (Svg, circle, g, polygon, image,
                     path, polyline, text_, animateTransform)
import Svg.Attributes exposing (..)
import Styles exposing (..)


type alias Model =
    { polygon : Polygon
    , stack : Stack Int
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
                top_idx = trust <| listCyclicGet -1 model.stack
                top = Polygon.getAt top_idx model.polygon
                scd_idx = trust <| listCyclicGet -2 model.stack
                scd = Polygon.getAt scd_idx model.polygon
                next = Polygon.getAt model.next_point model.polygon
                is_not_ccw = ccwTest scd top next < 1
            in
                case (is_not_ccw, model.next_point) of
                    (True, 1) ->
                        let
                            new_stack =
                                model.stack
                                |> List.tail |> trust 
                                |> Stack.pop |> Tuple.second
                                |> Stack.push model.next_point
                        in
                        { model
                         | stack = new_stack
                         , phase = Done
                        }
                    (True, _) ->
                        { model
                         | stack = Tuple.second <| Stack.pop model.stack
                        }
                    (False, 1) ->
                        { model
                         | next_point = remainderBy (List.length model.polygon)
                                                    (model.next_point+1)
                         , phase = Done
                        }
                    (False, _) ->
                        { model
                         | stack = Stack.push model.next_point model.stack 
                         , next_point = remainderBy (List.length model.polygon) (model.next_point+1)
                        }
        Done ->
            model


-- initialization step of the algorithm
initState : Model -> Model
initState model =
    let
        oriented_polygon = if Polygon.isCCW model.polygon
                           then model.polygon
                           else List.reverse model.polygon
        shifted_polygon = Polygon.restartAtCCW oriented_polygon
    in
    { model 
      | polygon = shifted_polygon
      , next_point = 2
      , stack = [0,1]
      , phase = InProgress
    }


started_desc : Html msg
started_desc =
    div
        []
        [ p []
            [ text "Since we're given a "
            , i [] [ text "simple polygon " ]
            , text ("our edges shouldn't overlap, and we'll just assume we were given our polygon "
                 ++ "in counter-clockwise (CCW) order. If it isn't, we'll just reverse the point "
                 ++ "in counter-clockwise (CCW) order. If it isn't, we'll just reverse the point "
                 ++ "order. After we know it's in CCW order, we'll identify the bottom-left-most "
                 ++ "point, we'll "
                 ++ "start there and call it '0'. We'll even number the rest of the points "
                 ++ "in CCW order going up from 0, to 1, then 2, etc. You can see we've "
                 ++ "already relabeled them. ")
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


-- describe what the algorithm would do this step
describeStep : Model -> Html m
describeStep model =
    case model.phase of
        NotStartedYet ->
            started_desc
        _ ->
            let
                top_idx = trust <| listCyclicGet -1 model.stack
                top = Polygon.getAt top_idx model.polygon
                scd_idx = trust <| listCyclicGet -2 model.stack
                scd = Polygon.getAt scd_idx model.polygon
                next = Polygon.getAt model.next_point model.polygon
                is_not_ccw = ccwTest scd top next < 1
            in
            case (is_not_ccw, model.next_point) of
                (True, 1) ->
                     writePointAction "Removed and popped point 0, then pushed point"
                                      next
                                      model.next_point
                (True, _) ->
                     writePointAction "Popped point" top top_idx
                (False, 1) ->
                     writePointAction "Finished at point" top top_idx
                (False, _) ->
                     writePointAction "Pushed point" next model.next_point


initEmptyState : Polygon -> Model
initEmptyState polygon =
    { polygon = polygon
    , stack = []
    , phase = NotStartedYet
    , next_point = 0
    }


-- an example of a polygon for which this algorithm fails
counter_example : Polygon
counter_example =
    [ (-15, -15)
    , (15, -15)
    , (21.70, 3.23)
    , (11.84, -6.72)
    , (15.55, 3.55)
    , (8.24, 6.20)
    , (27.75, 28.26)
    , (4.74, 24.23)
    , (-0.02, 20.20)
    , (0.92, 11.40)
    , (3.79, 8.11)
    , (6.12, -1.00)
    , (2.20, -3.22)
    , (2.51, -9.69)
    , (-4.90, -9.69)
    , (-7.87, -5.03)
    , (-4.26, 0.79)
    , (-9.25, 4.82)
    , (-6.60, 16.81)
    , (-15, 15)
    ]


drawState : Model -> Svg msg
drawState model =
    g []
      (
          -- TODO: move to constants
      [ Svg.path [ d "M -36 0 v -20 h 5 v 20"
                 , fill "none"
                 , stroke "grey" ]
                 []
      ] ++ List.indexedMap
             (\i n -> text_ [ x "-33.5"
                            , y <| String.fromInt (18 - 4*i)
                            , class "stack-entry"
                            , cartesian_flip
                            ]
                            [ text <| String.fromInt n ])
             model.stack
      )


drawHull : Model -> Svg msg
drawHull model =
    let
        hull =
            model.stack
            |> List.map (\n -> Polygon.getAt n model.polygon)
    in
    polyline (Styles.hull
             [ points
                <| svgPointsFromList
                <| hull
             ]
             ) []

drawStep : Model -> Svg msg
drawStep model =
    let
        top = Polygon.getAt (trust <| listCyclicGet -1 model.stack) model.polygon
        scd = Polygon.getAt (trust <| listCyclicGet -2 model.stack) model.polygon
        next = Polygon.getAt model.next_point model.polygon
        (next_x, next_y) = next
        ccw_triangle = [scd, top, next]
        (ccw_x, ccw_y) = Polygon.midpoint ccw_triangle
    in
    g []
      [ Svg.polygon [ fill ccw_triangle_stroke
                    , fillOpacity "0.3"
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
      , circle [ fill next_point_color
               , cx <| String.fromFloat next_x
               , cy <| String.fromFloat next_y
               , r point_radius
               ]
               []
      ]
