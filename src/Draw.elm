module Draw exposing (view)

-- Imports

import Browser
import Collage exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Html exposing (Html, a, button, div, table, td, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import List exposing (isEmpty)
import Tuple


main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { pts : List Point
    , start : Point
    , next : Point
    }


type Msg
    = Next
    | Stop


type alias Point =
    ( Int, Int )


init : ( Model, Cmd Msg )
init =
    ( { pts = []
      , start = ( 0, 0 )
      , currPoint = ( 0, 0 )
      }
    , Cmd.none
    )



-- Update


addPoint model =
    case model.pts of
        [] ->
            -- There's no points yet
            ( { model | pts = [ model.next ], start = model.next }, Cmd.none )

        _ ->
            if near model.next model.start then
                ( { model | pts = model.pts }, Stop )

            else
                ( { model | pts = model.next :: model.pts }, Cmd.none )


finishShape model =
    -- Change from a bunch of polylines to a polygon
    polygon model.pts
        |> styled ( uniform gray, solid thick (uniform black) )
            ( { model | pts = model.pts }, Cmd.none )


update msg model =
    case msg of
        Next ->
            addPoint model

        Stop ->
            finishShape model



-- View


draw model =
    if isEmpty model.pts then
        []

    else
        path model.pts
            |> traced (uniform blue)


view model =
    draw model
        |> scale 200
        |> svg



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Html.Events.onClick (D.succeed Next)



-- Helpers


near p p_ =
    10 > dist p p_


dist ( x, y ) ( x_, y_ ) =
    sqrt <| (x - x_) ^ 2 + (y - y_) ^ 2
