module IfExpressions exposing (bar, foo, foobar)


foobar : Bool -> Bool
foobar b =
    if b then
        True

    else if not b then
        False

    else
        False


foo : Bool -> Bool
foo b =
    if b then
        True

    else
        False


bar : Bool -> Bool
bar b =
    if b then
        False

    else
        True
