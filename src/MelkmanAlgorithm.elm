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
import Utils exposing (writePointAction, trust, listCyclicGet)


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
                    model
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
                top_idx = trust <| listCyclicGet -1 model.deque
                top = Polygon.getAt top_idx model.polygon
                scd_idx = trust <| listCyclicGet -2 model.deque
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
    , deque = []
    , phase = NotStartedYet
    , next_point = 0
    }
