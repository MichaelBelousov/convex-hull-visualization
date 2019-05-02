{-
Module containing utilities for working with polygons
-}

module Polygon exposing
    ( Polygon
    , isCCW
    , midpoint
    , getAt
    , restartAtCCW
    , getEdges
    )


import Utils exposing (trust, listCyclicGet)
import Polyline
import List.Extra exposing (last, getAt)
import Geometry exposing (Point, Edge, ccwTest)


type alias Polygon = List Point


-- returns whether a polygon is in CCW order
isCCW : Polygon -> Bool
isCCW polygon =
    let
        edges = getEdges polygon
        result =
            edges
            |> List.map (\((x1,y1),(x2,y2))
                        -> (x2-x1)*(y2+y1))
            |> List.sum
    in
        if result < 0 then True
                      else False


-- get the averaged midpoint of a polygon
midpoint : Polygon -> Point
midpoint polygon =
    let
        xsum = List.sum <| List.map Tuple.first polygon
        ysum = List.sum <| List.map Tuple.second polygon
        len = List.length polygon
    in
        ( xsum / toFloat len
        , ysum / toFloat len)


{-
get the nth vertex of a polygon in its order,
allowing negative and cyclic indices
-}
getAt : Int -> Polygon -> Point
getAt n polygon =
    trust <| listCyclicGet n polygon


-- shift a CCW polygon until it starts with three CCW points
restartAtCCW : Polygon -> Polygon
restartAtCCW polygon =
    case polygon of
        a::b::c::rest ->
            if ccwTest a b c == 1
            then polygon
            else restartAtCCW <| b::c::rest ++ [a]
        _ ->
            Debug.todo "bad polygon?"


-- get the edges belonging to a polygon
getEdges : Polygon -> List (Point, Point)
getEdges polygon =
    (Polyline.getEdges polygon)
    ++ [(trust <| last polygon,
         trust <| List.head polygon)]
