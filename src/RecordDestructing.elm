module RecordDestructing exposing (..)


type alias Person =
    { name : String
    , age : Int
    }


viewAge : Person -> String
viewAge { age } =
    String.fromInt age


foo : Person -> Int
foo a =
    a.age


negative : Person -> ( String, Int )
negative p =
    ( p.name, .age p )


negative2 : Person -> ( String, Int )
negative2 { name, age } =
    ( name, age )
