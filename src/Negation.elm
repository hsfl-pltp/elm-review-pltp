module Negation exposing (..)

foo : Int -> Int -> Bool
foo a b = 
    not (a == b || b >= 7)

num : Int -> Int
num a =
    a + 2

bar : Int -> Int -> Bool
bar a b =
    not (a == num b) && not (b + 3 >= 7)