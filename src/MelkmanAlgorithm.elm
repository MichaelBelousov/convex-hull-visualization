{-
Module containing logic and types for Naive polygon convex hull finding algorithm
-}

module MelkmanAlgorithm exposing 
    ( stepState
    , describeStep
    , Model
    , initEmptyState
    )


import Html exposing (Html, text, p, div, i)
import Polygon exposing (Polygon)
import Utils exposing (listCyclicGet)
import Geometry exposing (ccwTest)
import Algorithm exposing (..)
import Html exposing (Html)
import Deque exposing (Deque)
import Utils exposing (writePointAction, trust)


type alias Model =
    { polygon : Polygon
    , stack : Deque Int
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
                            removed_zero = trust <| List.tail model.stack
                            popped_zero = Tuple.second <| pop removed_zero
                            pushed_next = push popped_zero model.next_point
                        in
                        { model
                         | stack = pushed_next
                         , phase = Done
                        }
                    (True, _) ->
                        { model
                         | stack = Tuple.second <| pop model.stack
                        }
                    (False, 1) ->
                        { model
                         | next_point = remainderBy (List.length model.polygon)
                                                    (model.next_point+1)
                         , phase = Done
                        }
                    (False, _) ->
                        { model
                         | stack = push model.stack model.next_point
                         , next_point = remainderBy (List.length model.polygon) (model.next_point+1)
                        }
        Done ->
            model


-- initialization step of the algorithm
initState : Model -> Model
initState model =
    let
        start_deque = []
        |> Deque.push
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
