module Stack exposing (Stack, pop, push)

import List exposing (reverse)

-- Types

type alias Stack a = List a

-- Functions

pop : Stack a -> (Maybe a, Stack a)
pop stack =
    case List.reverse stack of
        last::rest -> (Just last, reverse rest)
        [] -> (Nothing, [])

push : Stack a -> a -> Stack a
push stack item =
    stack ++ [item]
