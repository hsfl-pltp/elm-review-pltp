module UnqualifiedImport exposing (..)

import Html exposing (div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (map)

a : List Int
a =
    map (\i -> i + 3) [ 1, 2, 3 ]
