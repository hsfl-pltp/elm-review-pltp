module EtaReduction exposing (..)


incList : List Int -> List Int
incList list =
    List.map inc list


inc : Int -> Int
inc =
    \i -> i - 1


dec : Int -> Int
dec =
    \d -> d - 1


increase1 : Int -> Int
increase1 =
    \x -> inc x


increase2 : Int -> Int
increase2 =
    \x -> inc x


moreArgs : Int -> Int -> Int
moreArgs =
    \x y -> add (add x y) y


foo : Int -> Int
foo =
    \x -> add x x
