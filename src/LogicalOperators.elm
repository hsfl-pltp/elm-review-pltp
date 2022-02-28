module LogicalOperators exposing (..)


any : (a -> Bool) -> List a -> Bool
any isOkay list =
    case list of
        [] ->
            False

        x :: xs ->
            if isOkay x then
                True

            else
                any isOkay xs


any2 : (a -> Bool) -> List a -> Bool
any2 isOkay list =
    case list of
        [] ->
            False

        x :: xs ->
            isOkay x || any isOkay xs


func1 : Bool -> Bool
func1 a =
    if 4 < 2 then
        True

    else
        a


func2 : Bool -> Bool
func2 a =
    if 4 < 2 then
        False

    else
        a


func3 : Bool -> Bool
func3 a =
    if 4 < 2 then
        a

    else
        True


func4 : Bool -> Bool
func4 a =
    if 4 < 2 then
        a

    else
        False


c : Bool
c =
    True


func5 : Bool -> Bool
func5 a =
    if 4 < 2 then
        a

    else
        c
