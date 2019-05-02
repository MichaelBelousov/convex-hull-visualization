{-
Module containing random utilities
-}

module Utils exposing
    ( trust
    , makeCube
    , listCyclicGet
    , writePointAction
    , pointToString
    )


import Geometry exposing (Point)
import List.Extra
import Html exposing (Html, ul, li, text)


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
    [ (-half_sz, -half_sz)
    , ( half_sz, -half_sz)
    , ( half_sz,  half_sz)
    , (-half_sz,  half_sz)
    ]


-- get a list member, cycling on bounds
listCyclicGet : Int -> List a -> Maybe a
listCyclicGet n list =
    case list of
        [] ->
            Nothing
        _ ->
            let
                len = List.length list
                rel_idx = if n < 0 then len - (abs n)
                                   else n
                idx = remainderBy len rel_idx
            in
            List.Extra.getAt idx list


-- construct an html note of an action to a point
writePointAction : String -> Point -> Int -> Html msg
writePointAction action (x,y) index =
    ul []
       [ li []
            [ text
               (  action ++ ": "
               ++ String.fromInt index ++ " at ("
               ++ pointToString (x,y) ++ ")"
               )
            ]
       ]


-- construct a representative string from a point tuple
pointToString : Point -> String
pointToString (x, y) =
    String.fromFloat (toFloat(round(x * 100)) / 100.0)
    ++ ", "
    ++ String.fromFloat (toFloat(round(y * 100)) / 100.0)
