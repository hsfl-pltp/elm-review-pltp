module FunctionNames exposing (getString)

getString : Int -> String
getString n =
 case n of
     1 -> "1"
     _ -> "more"
