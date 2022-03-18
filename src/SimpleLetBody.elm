module SimpleLetBody exposing (..)


foo : Int -> Int -> Int
foo a b =
    let
        c =
            a + b
    in
    c
