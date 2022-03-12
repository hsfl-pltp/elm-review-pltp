module ForbiddenFeatures exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (class)


applyForbiddenOperator : Int -> String
applyForbiddenOperator i =
    i |> String.fromInt


mapList : List String -> List String
mapList list =
    List.map (\e -> "value: " ++ e) list


buttonStyle : List (Attribute msg)
buttonStyle =
    [ class "button-primary" ]



type Test
    = Test1 Bool
    | Test2 Int


foo : Int -> String
foo bar =
    bar |> String.fromInt


toString : List Int -> List String
toString list =
    List.map String.fromInt list


letIn : List Int -> List String
letIn list =
    let
        a =
            toString list
    in
    a
