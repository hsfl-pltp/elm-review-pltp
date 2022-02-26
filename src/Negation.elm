module Negation exposing (..)


foo : Int -> Int -> Bool
foo a b =
    not (a == (+) b 3 || b >= -(a + b))


bar : Int -> Int -> Bool
bar a b =
    not (a == b - 2) || not (b >= 7)


baz : Int -> Int -> Int -> Bool
baz a b c =
    not ((a < b) == (a < c))


func1 : Bool -> Bool -> Bool
func1 a b =
    a || b


func2 : Bool -> Bool -> Bool
func2 a b =
    not
        (func1 a b
            && (if b then
                    a

                else
                    not a
               )
        )


func3 : Int -> Int -> Bool
func3 a b =
    not (add a b > a)


func4 : Int -> Int -> Bool
func4 a b =
    not (List.foldl add 0 [ a, b ] > (+) -9 4)


func5 : Bool -> Bool -> Bool
func5 a b =
    not (List.foldl func1 False [ a, b ] && (a || b))


func6 : Bool
func6 =
    not ((0xF0 > 0x1A) && True)


func7 : Bool
func7 =
    not (True && Tuple.first ( 'a', "string" ) == 'a')


func8 : Bool
func8 =
    not (True && helpFunc8 { name = "test", number = 5 })


type alias TypeFunc8 =
    { name : String
    , number : Int
    }


helpFunc8 : TypeFunc8 -> Bool
helpFunc8 tf =
    False
