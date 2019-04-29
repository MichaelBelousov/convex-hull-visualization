{-
Ports for interacting with live SVGs
-}
port module SvgPorts exposing (mouseToSvgCoords)

import Json.Encode

    -- implementation in index.html
port mouseToSvgCoords : (Json.Encode.Value -> m) -> Sub m
