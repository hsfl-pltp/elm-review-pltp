module RecordDestructing exposing (..)


type alias Person =
    { name : String
    , age : Int
    }


viewAge : Person -> String
viewAge { age } =
    String.fromInt age
