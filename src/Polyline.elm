{-
Module containing utilities for working with polylines
-}
module Polyline exposing (Polyline, getEdges)

import Utils exposing (trust)
import Geometry exposing (Point, Edge)

type alias Polyline = List Point

-- get the edges of the polyline
getEdges : Polyline -> List (Point, Point)
getEdges polyline =
    List.map2 (\p q -> (p,q))
              polyline
              (trust <| List.tail polyline)
