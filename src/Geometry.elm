{-
Module containing basic geometric logic and types
-}
module Geometry exposing (Point, Edge, ccwTest)

type alias Point = ( Float, Float )
type alias Edge = ( Point, Point )

-- CCW formula
ccwTest : Point -> Point -> Point -> Int
ccwTest (ax,ay) (bx,by) (cx,cy) =
    let
        value = (ax * (by - cy)) - (bx * (ay - cy)) + (cx * (ay - by))
    in
        if value > 0 then 1
        else if value < 0 then -1
        else 0
