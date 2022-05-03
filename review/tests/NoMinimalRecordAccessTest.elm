module NoMinimalRecordAccessTest exposing (all)

import NoMinimalRecordAccessOld exposing (rule)
import Review.Test exposing (ExpectedError)
import Test exposing (Test, describe, test)


ruleConfig : NoMinimalRecordAccessOld.Config
ruleConfig =
    { threshold = 2
    , ignoreFunctions = []
    }


all : Test
all =
    describe "NoMinimalRecordDestructing"
        [ test "should report an error when a minimal record destructing is used" <|
            \() ->
                destructingSource
                    |> Review.Test.run (rule ruleConfig)
                    |> Review.Test.expectErrors
                        [ destructingError 2 ]
        , test "should report an error when to less components used from a record by access" <|
            \() ->
                accessSource
                    |> Review.Test.run (rule ruleConfig)
                    |> Review.Test.expectErrors
                        [ accessError 2 ]
        ]


destructingError : Int -> ExpectedError
destructingError threshold =
    Review.Test.error
        { message = "To few components used from record destructing"
        , details =
            [ "If you destruct " ++ String.fromInt threshold ++ "or less components from a record, then you sould give the components as parameters to you function"
            , "For example, the record {name: String, age: Int}. The components name and age should be given as arguments to the function."
            ]
        , under = "{ name }"
        }


accessError : Int -> ExpectedError
accessError threshold =
    Review.Test.error
        { message = "To few compoents used in record access"
        , details = [ "If you use " ++ String.fromInt threshold ++ " or less components from a record, you should move the components as arguments to the function" ]
        , under = "\"The name: \" ++ person.name ++ \", age is \" ++ String.fromInt person.age"
        }


destructingSource : String
destructingSource =
    """
module Foo exposing (..)

type alias Person = 
    {name: String, age: Int}

viewName : Person -> String
viewName { name } =
    "The name of the person is:" ++ name
"""


accessSource : String
accessSource =
    """
module Food exposing (..)

type alias Person = 
    {name: String, age: Int}

viewName : Person -> String
viewName person =
    "The name: " ++ person.name ++ ", age is " ++ String.fromInt person.age
    """
