module UseEtaReductionsTest exposing (all)

import UseEtaReductions exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "UseEtaReductions"
        [ test "should report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "REPLACEME"
                            }
                        ]
        ]
