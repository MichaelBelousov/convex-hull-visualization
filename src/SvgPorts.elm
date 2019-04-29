{-
Ports for interacting with live SVGs
-}
port module SvgPorts exposing (..)

import Json.Encode
import Json.Decode as Decode exposing (Decoder)

    -- implementation in index.html
port mouseToSvgCoords : (Json.Encode.Value -> m) -> Sub m

type alias SvgPoint =
    { x : Float
    , y : Float
    }

decodeSvgPoint : Decoder SvgPoint
decodeSvgPoint =
    Decode.map2 SvgPoint
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)

