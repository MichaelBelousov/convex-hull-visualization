{-
Ports for interacting with enter and spacebar
-}
port module KeyUpPorts exposing (..)

import Json.Encode
import Json.Decode as Decode exposing (Decoder)

    -- implementation in index.html

port keyUp : (Json.Encode.Value -> m) -> Sub m
