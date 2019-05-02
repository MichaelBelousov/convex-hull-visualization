module Deque exposing
    ( Deque
    , pop
    , push
    , insert
    , remove
    )


type alias Deque a = List a


pop : Deque a -> (Maybe a, Deque a)
pop deque =
    case List.reverse deque of
        last::rest ->
            (Just last, List.reverse rest)
        [] ->
            (Nothing, [])


push : Deque a -> a -> Deque a
push deque item =
    deque ++ [item]


insert : Deque a -> a -> Deque a
insert deque item ->
    item::deque


remove : Deque a -> (Maybe a, Deque a)
remove deque =
    case deque of
        first::rest ->
            (Just first, rest)
        [] ->
            (Nothing, [])
