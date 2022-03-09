module EtaReduction exposing (..)


incList : List Int -> List Int
incList list =
    List.map inc list


inc : Int -> Int
inc =
    \i -> 1 + i


increase1 : Int -> Int
increase1 =
    \x -> inc x


increase2 : Int -> Int
increase2 =
    \x -> inc x
