module BoolComparison exposing (..)


boolComp : Bool -> Bool
boolComp b =
    if b == True then
        True

    else if b /= False then
        False

    else if False == b then
        False

    else
        False
