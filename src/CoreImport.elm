module CoreImport exposing (..)

import Basics exposing (..)
import Char exposing (Char)
import Debug
import List exposing ((::))
import Maybe exposing (Maybe(..))
import Platform exposing (Program)
import Platform.Cmd as Cmd exposing (Cmd)
import Platform.Sub as Sub exposing (Sub)
import Result exposing (Result(..))
import String exposing (String)
import Tuple


a : List String -> Char
a _ =
    'a'


b : Int
b =
    Debug.log "" 23


c : Program () () ()
c =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \model _ -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


d : Cmd Int
d =
    Cmd.none


e : Sub Int
e =
    Sub.none


f : ( (), () )
f =
    Tuple.pair () ()
