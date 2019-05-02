module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import Html exposing (Html, Attribute, div, button, text, a,
                      table, tr, td, p, i, b, ul, ol, li, span)
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
import NaiveAlgorithm
import MelkmanAlgorithm
import Drawing
import Styles exposing (cartesian_area)

-- Browser Model

type Msg
    = StepAlgorithm
    | Restart
    | BadPolygon
    | DoubleClickPoint Int
    | LeftClickEdge Int
    | GrabPoint Int
    | ReleasePoint
    | MouseMoved Encode.Value
    | ToggleNarration
    | DoNothing

-- update the grabbed point
updateGrab : Model -> Model
updateGrab model =
    case model.grabbed of
        Just grabbed ->
            let
                naive_state = model.naive_state
                new_naive_state =
                    { naive_state
                     | polygon = List.indexedMap
                                     (\i p -> if i == grabbed
                                             then model.mouse_in_svg
                                             else p)
                                     model.naive_state.polygon
                    }
            in
                { model | naive_state = new_naive_state }
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
            andScroll
            <| case (model.naive_state.phase, model.melkman_state.phase) of
                (NotStartedYet, _) ->
                    let
                        melkman_state = grabbed_moved.melkman_state
                        synced_melkman_state =
                            { melkman_state | polygon = grabbed_moved.naive_state.polygon }
                    in
                    { model
                     | naive_state = NaiveAlgorithm.stepState grabbed_moved.naive_state
                     , melkman_state = MelkmanAlgorithm.stepState synced_melkman_state
                     , progress_log = model.progress_log
                        ++ [MelkmanAlgorithm.describeStep synced_melkman_state]
                    }
                (Done, Done) ->
                    { model
                     | naive_state = NaiveAlgorithm.initEmptyState model.naive_state.polygon
                     , melkman_state = MelkmanAlgorithm.initEmptyState model.melkman_state.polygon
                    }
                _ ->
                    { model
                     | naive_state = NaiveAlgorithm.stepState grabbed_moved.naive_state
                     , melkman_state = MelkmanAlgorithm.stepState grabbed_moved.melkman_state
                     , progress_log = model.progress_log
                        ++ [MelkmanAlgorithm.describeStep grabbed_moved.melkman_state]
                    }
        Restart ->
            nocmd
            <| { model
                | naive_state = NaiveAlgorithm.initEmptyState model.naive_state.polygon
                , melkman_state = MelkmanAlgorithm.initEmptyState model.melkman_state.polygon
               }
        BadPolygon ->
            let
                naive_state = NaiveAlgorithm.initEmptyState NaiveAlgorithm.counter_example
            in
            nocmd <| { before_start_state
                      | naive_state = naive_state
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
        ToggleNarration ->
            nocmd <| { model | have_narration = not model.have_narration }
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
        "r" ->
            Restart
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
                                 (  viewVisualization model
                                 ++ viewNarration model
                                 )
                            ]
                     ]
              , div [ class "footer" ]
                    [ a [ href "about.html" ] [ text "about" ] ]
              ]
        ]
    }


viewVisualization : Model -> List (Html Msg)
viewVisualization model =
    case model.naive_state.phase of
        NotStartedYet ->
            [ td [ class "visualization-wrap" ]
                 [ div [ class "visualization" ]
                       [ drawNaiveAlgorithmState model.naive_state ] ]
            ]
        _ ->
            [ td [ class "left-visualization-wrap" ]
                 [ div [ class "left-visualization" ]
                       [ drawNaiveAlgorithmState model.naive_state ]
                 ]
            , td [ class "right-visualization-wrap" ]
                 [ div [ class "right-visualization" ]
                       [ drawMelkmanAlgorithmState model.melkman_state ]
                 ]
            ]


viewNarration : Model -> List (Html Msg)
viewNarration model =
    let
        (btn_label, btn_action) =
            case (model.naive_state.phase, model.melkman_state.phase) of
                (NotStartedYet, _) ->
                    ("start!", StepAlgorithm)
                (Done, Done) ->
                    ("restart", Restart)
                _ ->
                    ("next step", StepAlgorithm)
    in
        if model.have_narration
        then
            [ td [ class "narration" ]
                 [ div [ class "progress-log"
                       , id "progress-log"
                       ]
                       model.progress_log
                  , div [ class "next-btn-container" ]
                        [ button [ onClick btn_action ]
                                 [ text btn_label ]
                        , button [ onClick BadPolygon ]
                                 [ text "Bad Polygon" ]
                        , button [ onClick ToggleNarration ]
                                 [ text "Remove Narration" ]
                        ]
                 ]
            ]
        else []


-- TODO: move polygon out of algorithm!
type alias Model =
    { grabbed : Maybe Int
    , progress_log : List (Html Msg)
    , mouse_in_svg : Point
    , naive_state : NaiveAlgorithm.Model
    , melkman_state : MelkmanAlgorithm.Model
    , have_narration : Bool
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
        polygon = model.naive_state.polygon
        point = trust <| getAt point_idx polygon
        naive_state = model.naive_state
        new_naive_state = { naive_state | polygon = List.filter (\p -> p /= point) polygon }
    in
        if List.length polygon > 3
        then { model | naive_state = new_naive_state }
        else model


insertPoint : Model -> Int -> Model
insertPoint model edge_idx =
    let
        polygon = model.naive_state.polygon
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
        naive_state = model.naive_state
        new_naive_state = { naive_state | polygon = new_polygon }
    in
    { model
     | naive_state = new_naive_state
    }


    -- state when the app starts
before_start_state : Model
before_start_state =
    { grabbed = Nothing
    , progress_log = [intro]
    , mouse_in_svg = (0,0)
    , naive_state = NaiveAlgorithm.initEmptyState init_polygon
    , melkman_state = MelkmanAlgorithm.initEmptyState init_polygon
    , have_narration = True
    }


drawMelkmanAlgorithmState : MelkmanAlgorithm.Model -> Html Msg
drawMelkmanAlgorithmState model =
    let
        _ = Debug.log "melkman" model
        polygon = model.polygon
        svgBase extra =
            div [ class "resizable-svg-container" ]
                [ svg [ viewBox "-40 -12 80 30"
                      , Svg.Attributes.class "resizable-svg"
                      , cartesian_area
                      ]
                      (
                      [ Drawing.drawOrigin
                      , MelkmanAlgorithm.drawState model
                      , drawMelkmanAlgorithmPolygon model
                      , MelkmanAlgorithm.drawHull model
                      ] ++ extra ++
                      [ Drawing.drawVertsIndex model.polygon
                      ]
                      )
                 ]
    in
    case model.phase of
        InProgress ->
            svgBase [ MelkmanAlgorithm.drawStep model ]
        _ ->
            svgBase []

drawNaiveAlgorithmState : NaiveAlgorithm.Model -> Html Msg
drawNaiveAlgorithmState model =
    let
        _ = Debug.log "naive" model
        polygon = model.polygon
        svgBase extra =
            div [ class "resizable-svg-container" ]
                [ svg [ viewBox "-40 -12 80 30"
                      , Svg.Attributes.class "resizable-svg"
                      , cartesian_area
                      ]
                      (
                      [ Drawing.drawOrigin
                      , NaiveAlgorithm.drawState model
                      , drawNaiveAlgorithmPolygon model
                      , NaiveAlgorithm.drawHull model
                      ] ++ extra ++
                      [ Drawing.drawVertsIndex model.polygon
                      ]
                      )
                 ]
    in
    case model.phase of
        InProgress ->
            svgBase [ NaiveAlgorithm.drawStep model ]
        _ ->
            svgBase []


-- Draw the polygon, return svg message
drawMelkmanAlgorithmPolygon : MelkmanAlgorithm.Model -> Svg Msg
drawMelkmanAlgorithmPolygon {phase, polygon} =
    let
        edge_attrs i =
            case phase of
                NotStartedYet ->
                    [ onMouseDown (LeftClickEdge i)
                    , onMouseUp   (ReleasePoint)
                    ]
                _ -> []
        vert_attrs i =
            case phase of
                NotStartedYet ->
                    [ onDoubleClick (DoubleClickPoint i)
                    , onMouseDown   (GrabPoint i)
                    , onMouseUp     (ReleasePoint)
                    ]
                _ -> []
    in
        Drawing.drawPolygon polygon edge_attrs vert_attrs

-- ew code dupe:
drawNaiveAlgorithmPolygon : NaiveAlgorithm.Model -> Svg Msg
drawNaiveAlgorithmPolygon {phase, polygon} =
    let
        edge_attrs i =
            case phase of
                NotStartedYet ->
                    [ onMouseDown (LeftClickEdge i)
                    , onMouseUp   (ReleasePoint)
                    ]
                _ -> []
        vert_attrs i =
            case phase of
                NotStartedYet ->
                    [ onDoubleClick (DoubleClickPoint i)
                    , onMouseDown   (GrabPoint i)
                    , onMouseUp     (ReleasePoint)
                    ]
                _ -> []
    in
        Drawing.drawPolygon polygon edge_attrs vert_attrs


main : Program () Model Msg
main =
    Browser.document
        { init = (\f -> ( before_start_state, Cmd.none))
        , view = view
        , update = (\msg model -> update msg model)
        , subscriptions = subscriptions
        }
