module TEA exposing (..)


update : Bool -> Int -> Int
update b i =
    if b then
        i

    else
        update (not b) i + 1


subscriptions : Int -> Sub Int
subscriptions i =
    Sub.none
