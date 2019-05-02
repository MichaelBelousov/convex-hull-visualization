{-
module containing utilties and constants
for styles
-}
module Styles exposing (..)


import Svg
import Svg.Attributes exposing (..)


cartesian_area = transform "scale(1, -1)"
cartesian_flip= transform "scale(1, -1)"
label_x_shift = dx "2.5"
label_y_shift = dy "2.5"
label_offset = 0.2
point_color = "blue"
point_radius = String.fromFloat 2
next_point_color = "yellow"
ccw_triangle_fill = "none"
ccw_triangle_stroke = "yellow"
ccw_triangle_stroke_width = String.fromFloat 0.7
ccw_triangle_stroke_dash = "3,2"
ccw_wheel_radius = 5
ccw_wheel_id = "ccw_wheel"
origin_fill = "none"
origin_stroke = "black"
origin_stroke_width = String.fromFloat 0.3


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
