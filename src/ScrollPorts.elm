{-
Ports for interacting with scrolling
-}

port module ScrollPorts exposing (scrollToBottom)


import Json.Encode
import Json.Decode as Decode exposing (Decoder)


-- scroll to the bottom of an element with by id
scrollToBottom: String -> Cmd m
scrollToBottom id =
    scrollToBottom_ <| Json.Encode.string id


-- implementation in index.html
port scrollToBottom_ : Json.Encode.Value -> Cmd m
