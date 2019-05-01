{-
Module containing random utilities
-}
module Utils exposing (trust, makeCube)

import Geometry exposing (Point)

type alias Polygon = List Point

-- trust that a Maybe is fulfilled
trust : Maybe a -> a
trust x =
    case x of
        Just y -> y
        Nothing -> Debug.todo "trust got Nothing"

-- make a cube from a given half-size
makeCube : Float -> Polygon
makeCube half_sz =
    [ (-half_sz,  -half_sz)
    , (half_sz, -half_sz)
    , (half_sz, half_sz)
    , (-half_sz, half_sz)
    ]
