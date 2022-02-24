module Negation exposing (..)

foo : Int -> Int -> Bool
foo a b = 
    not (a == b + 3 || b >= 7)

bar : Int -> Int -> Bool
bar a b =
    not (a == b - 2) || not (b >= 7)

baz : Int -> Int -> Int -> Bool
baz a b c =
    not ((a < b) == (a < c))

test1 : Bool -> Bool -> Bool
test1 a b =
    not ( a || b )