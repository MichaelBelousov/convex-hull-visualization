module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import Html exposing (Html, Attribute, div, button, text, a,
                      table, tr, td, p, i, b, ul, ol, li)
import Html.Events exposing (onClick, onDoubleClick, onMouseUp, onMouseDown)
import Html.Attributes exposing (..)
import List.Extra exposing (getAt, last, splitAt)
import Json.Decode as Decode
import Json.Encode as Encode
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (viewBox)
import SvgPorts exposing (mouseToSvgCoords, decodeSvgPoint)
import ScrollPorts exposing (scrollToBottom)
import Geometry exposing (Point, ccwTest)
import Polygon exposing (Polygon)
import Utils exposing (..)
import Algorithm exposing (..)
-- import NaiveAlgorithm as ConvexHullAlgorithm
import MelkmanAlgorithm as ConvexHullAlgorithm
import Drawing
import Styles exposing (cartesian_area)

-- Browser Model

type Msg
    = StepAlgorithm
    | DoubleClickPoint Int
    | LeftClickEdge Int
    | GrabPoint Int
    | ReleasePoint
    | MouseMoved Encode.Value
    | DoNothing

-- update the grabbed point
updateGrab : Model -> Model
updateGrab model =
    case model.grabbed of
        Just grabbed ->
            let
                algo_state = model.algo_state
                new_algo_state =
                    { algo_state
                     | polygon = List.indexedMap
                                     (\i p -> if i == grabbed
                                             then model.mouse_in_svg
                                             else p)
                                     model.algo_state.polygon
                    }
            in
                { model | algo_state = new_algo_state }
        Nothing ->
            model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        grabbed_moved = updateGrab model
        andScroll model_ = ( model_, scrollToBottom "progress-log" )
        nocmd model_ = ( model_, Cmd.none )
    in
    case msg of
        StepAlgorithm ->
            andScroll <| case model.algo_state.phase of
                Done ->
                    { model
                     | algo_state = ConvexHullAlgorithm.initEmptyState model.algo_state.polygon
                    }
                _ ->
                    { model
                     | algo_state = Debug.log "model" <| ConvexHullAlgorithm.stepState grabbed_moved.algo_state
                     , progress_log = model.progress_log
                        ++ [ConvexHullAlgorithm.describeStep grabbed_moved.algo_state]
                    }
        DoubleClickPoint point_idx ->
            nocmd <| deletePoint model point_idx
        LeftClickEdge edge_idx ->
            let
                insert_done = insertPoint grabbed_moved edge_idx
            in
                nocmd <| { insert_done | grabbed = Just (edge_idx+1) }
        GrabPoint point_idx ->
            nocmd <| { grabbed_moved | grabbed = Just point_idx }
        ReleasePoint ->
            nocmd <| { grabbed_moved | grabbed = Nothing }
        MouseMoved received ->
            nocmd <|
            case Decode.decodeValue decodeSvgPoint received of
                Ok {x, y} ->
                    { grabbed_moved | mouse_in_svg = (x,y) }
                Err _ ->
                    Debug.todo "bad value sent over svgCoords port sub"
        DoNothing ->
            nocmd <| model


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map whichKey (Decode.field "key" Decode.string)


whichKey : String -> Msg
whichKey string =
    case string of
        "Enter" ->
            StepAlgorithm
        " " ->
            StepAlgorithm
        _ ->
            DoNothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ mouseToSvgCoords MouseMoved
        , onKeyUp keyDecoder
        ]

view : Model -> Browser.Document Msg
view model =
    { title = app_title
    , body =
        [ div []
              [ div []
                    [ table [ style  "width" "100%"
                            , style "table-layout" "fixed"
                            ]
                            [ tr []
                                 [ viewVisualization model
                                 , viewNarration model
                                 ]
                            ]
                     ]
              , div [ class "footer" ]
                    [ a [ href "about.html" ] [ text "about" ] ]
              ]
        ]
    }


viewVisualization : Model -> Html Msg
viewVisualization model =
    td [ class "visualization" ]
       [ div [] [ drawConvexHullAlgorithmsState model ] ]


viewNarration : Model -> Html Msg
viewNarration model =
    let
        btn_label = case model.algo_state.phase of
            NotStartedYet ->
                "start!"
            InProgress ->
                "next step"
            Done ->
                "restart"
    in
        td [ class "narration" ]
           [ div [ class "progress-log"
                 , id "progress-log"
                 ]
                 model.progress_log
            , div [ class "next-btn-container" ]
                  [ button [ onClick StepAlgorithm ]
                           [ text btn_label ]
                  ]
           ]


type alias Model =
    { grabbed : Maybe Int
    , progress_log : List (Html Msg)
    , mouse_in_svg : Point
    , algo_state : ConvexHullAlgorithm.Model
    }


app_title = "Polygon Convex Hull"
init_polygon = makeCube 15

-- NOTE: generate z-order constants from a priority list?

-- UI Strings

intro : Html Msg
intro = div
        []
        [ p []
            [ text "Welcome. Together, we're going to find the "
            , i [] [ text "convex hull " ]
            , text "of this "
            , i [] [ text "simple polygon " ]
            , text ("on the left. A polygon is just a sequence of points, "
                 ++ "with straight edges between each sequential pair, such "
                 ++ "as a square, or arrow shape. You've probably heard of "
                 ++ "polygons before. A convex hull is what you get when you "
                 ++ "put a rubber band around one.  it's like the same polygon "
                 ++ "but you've removed all the dents.")
            ]
        , p []
            [ text ("You can ignore the grey container "
                 ++ "on the left, we'll use it soon, and that plus sign "
                 ++ "is just the origin for your reference. "
                 ++ "For now, feel free to build your own polygon by "
                 ++ "interacting with the available tools. "
                 ++ "When you're done, hit the 'Start!' button.")
            ]
        , ul []
             [ li [] [ text "Click and drag on points to move them around"]
             , li [] [ text "Double click on a point to delete it"]
             , li [] [ text "Click and drag on edges to add points"]
             ]
        , p []
            [ text ("If you haven't looked it up already, a simple polygon "
                 ++ "doesn't have any of its edges intersecting or overlapping. "
                 ++ "If you decide to do that, you will break the algorithms, "
                 ++ "but it's worth experimenting with anyway.")
            ]
        ]


deletePoint : Model -> Int -> Model
deletePoint model point_idx =
    let
        polygon = model.algo_state.polygon
        point = trust <| getAt point_idx polygon
        algo_state = model.algo_state
        new_algo_state = { algo_state | polygon = List.filter (\p -> p /= point) polygon }
    in
        if List.length polygon > 3
        then { model | algo_state = new_algo_state }
        else model


insertPoint : Model -> Int -> Model
insertPoint model edge_idx =
    let
        polygon = model.algo_state.polygon
        (front, back) = splitAt (edge_idx+1) polygon
        mdpt = case (last front, List.head back) of
                (Just x, Just y) ->
                    Polygon.midpoint [x, y]
                (Just x, Nothing) ->
                    Polygon.midpoint [ trust <| List.head front
                                     , trust <| last front
                                     ]
                (Nothing, Just y) ->
                    Polygon.midpoint [ trust <| List.head back
                                     , trust <| last back
                                     ]
                _ -> Debug.todo "bad polygon"
        new_polygon = front ++ [mdpt] ++ back
        algo_state = model.algo_state
        new_algo_state = { algo_state | polygon = new_polygon }
    in
    { model | algo_state = new_algo_state }


    -- state when the app starts
before_start_state : Model
before_start_state =
    { grabbed = Nothing
    , progress_log = [intro]
    , mouse_in_svg = (0,0)
    , algo_state = ConvexHullAlgorithm.initEmptyState init_polygon
    }


drawConvexHullAlgorithmsState : Model -> Html Msg
drawConvexHullAlgorithmsState model =
    let
        polygon = model.algo_state.polygon
        svgBase extra =
            div [ class "resizable-svg-container" ]
                [ svg [ viewBox "-40 -12 80 30"
                      , Svg.Attributes.class "resizable-svg"
                      , cartesian_area
                      ]
                      (
                      [ Drawing.drawOrigin
                      , ConvexHullAlgorithm.drawState model.algo_state
                      , drawPolygon model
                      , ConvexHullAlgorithm.drawHull model.algo_state
                      ] ++ extra ++
                      [ Drawing.drawVertsIndex model.algo_state.polygon
                      ]
                      )
                 ]
    in
    case model.algo_state.phase of
        InProgress ->
            svgBase [ ConvexHullAlgorithm.drawStep model.algo_state ]
        _ ->
            svgBase []


-- Draw the polygon, return svg message
drawPolygon : Model -> Svg Msg
drawPolygon model =
    let
        edge_attrs i =
            case model.algo_state.phase of
                NotStartedYet ->
                    [ onMouseDown (LeftClickEdge i)
                    , onMouseUp   (ReleasePoint)
                    ]
                _ -> []
        vert_attrs i =
            case model.algo_state.phase of
                NotStartedYet ->
                    [ onDoubleClick (DoubleClickPoint i)
                    , onMouseDown   (GrabPoint i)
                    , onMouseUp     (ReleasePoint)
                    ]
                _ -> []
    in
        Drawing.drawPolygon model.algo_state.polygon edge_attrs vert_attrs


main : Program () Model Msg
main =
    Browser.document
        { init = (\f -> ( before_start_state, Cmd.none))
        , view = view
        , update = (\msg model -> update msg model)
        , subscriptions = subscriptions
        }
