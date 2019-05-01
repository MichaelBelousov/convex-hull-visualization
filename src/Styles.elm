{-
module containing utilties and constants
for styles
-}
module Styles exposing
    ( polygon
    --, nextPoint
    , hull
    --, testTriangle
    --, origin
    )


import Svg
import Svg.Attributes exposing (..)


-- styles of the polygon
polygon : List (Svg.Attribute m) -> List (Svg.Attribute m)
polygon other_attrs =
    [ fill "none"
    , stroke "blue"
    , strokeWidth <| String.fromFloat 1.5
    , strokeLinecap "round"
    ]
    ++ other_attrs


-- styles of the convex hull
hull : List (Svg.Attribute m) -> List (Svg.Attribute m)
hull other_attrs =
    [ fill "none"
    , stroke "red"
    , strokeWidth <| String.fromFloat 2
    , strokeLinecap "round"
    ]
    ++ other_attrs
