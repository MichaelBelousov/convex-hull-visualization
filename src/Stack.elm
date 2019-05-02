module Stack exposing (Stack, pop, push)


type alias Stack a = List a


pop : Stack a -> (Maybe a, Stack a)
pop stack =
    case List.reverse stack of
        last::rest -> (Just last, List.reverse rest)
        [] -> (Nothing, [])


push : a -> Stack a -> Stack a
push item stack =
    stack ++ [item]
