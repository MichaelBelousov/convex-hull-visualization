{-
Module with drawing logic
-}
module Drawing exposing (..)


import Svg exposing (..)
import Svg.Attributes exposing (..)
import Geometry exposing (Point, Edge)
import Polygon exposing (Polygon)
import Polyline exposing (Polyline)
import Math.Vector2 as Vec2D exposing (Vec2, vec2)
import Styles exposing (..)


drawVertsIndex : Polygon -> Svg msg
drawVertsIndex polygon =
    g [] <| List.indexedMap
            (\i (vx,vy) ->
                let
                    (prev_x, prev_y) = Polygon.getAt (i-1) polygon
                    prev = Vec2D.vec2 prev_x prev_y
                    (curr_x, curr_y) = Polygon.getAt i polygon
                    curr = Vec2D.vec2 curr_x curr_y
                    (next_x, next_y) = Polygon.getAt (i+1) polygon
                    next = Vec2D.vec2 next_x next_y
                    -- centered around origin
                    cprev = Vec2D.sub prev curr
                    cnext = Vec2D.sub next curr
                    clabel = Vec2D.scale (-Styles.label_offset) -- NOTE: could clamp by normalized + offset
                            <| Vec2D.scale 0.5 <| Vec2D.add cprev cnext
                    label = Vec2D.add clabel curr
                    (label_x, label_y) = (Vec2D.getX label, Vec2D.getY label)
                in
                    text_ [ x <| String.fromFloat label_x
                          , y <| String.fromFloat (-label_y)
                          , class "point-label"
                          , Styles.cartesian_flip
                          ]
                          [ text <| String.fromInt i ]
            )
            polygon


drawPolygonEdges : Polygon -> (Int -> List (Attribute m)) -> List (Svg m)
drawPolygonEdges polygon interactions =
    List.indexedMap
        (\i ((x1_,y1_),(x2_,y2_)) ->
                line (Styles.polygon
                     ([ x1 <| String.fromFloat x1_, y1 <| String.fromFloat y1_
                      , x2 <| String.fromFloat x2_, y2 <| String.fromFloat y2_
                      ] ++ interactions i)
                     )
                     [])
        (Polygon.getEdges polygon)


drawPolygonVerts : Polygon -> (Int -> List (Attribute m)) -> List (Svg m)
drawPolygonVerts polygon interactions =
    List.indexedMap
        (\i (x,y) ->
                circle (
                       [ fill point_color
                       , cx <| String.fromFloat x
                       , cy <| String.fromFloat y
                       , r point_radius
                       ] ++ interactions i
                       )
                       []
                       )
        polygon


type alias IndexedAttributeFactory m =
   Int -> List (Svg.Attribute m)


-- Draw the polygon, return svg message
drawPolygon : Polygon -> IndexedAttributeFactory m -> IndexedAttributeFactory m -> Svg m
drawPolygon polygon edge_attrs vert_attrs =
    g []
      (  drawPolygonEdges polygon edge_attrs
      ++ drawPolygonVerts polygon vert_attrs
      )


drawOrigin : Svg msg
drawOrigin =
    Svg.path [ d "M 0 0 h 5 h -10 h 5 v 5 v -10"
             , fill Styles.origin_fill
             , stroke Styles.origin_stroke
             , strokeWidth Styles.origin_stroke_width
             ]
             []
