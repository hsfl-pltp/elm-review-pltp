module UseLogicalOperators exposing (rule)

{-| Forbids the use of if, when one path returns a bool value

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Helper
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports an error when one path of en if expression returns a bool value

    config =
        [ UseLogicalOperators.rule
        ]


## Fail

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


## Success

    any : (a -> Bool) -> List a -> Bool
    any isOkay list =
        case list of
            [] ->
                False

            x :: xs ->
                isOkay x || any isOkay xs

-}
initialContext : Rule.ContextCreator () ModuleNameLookupTable
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> lookupTable)
        |> Rule.withModuleNameLookupTable


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "UseLogicalOperators" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> ModuleNameLookupTable -> ( List (Error {}), ModuleNameLookupTable )
expressionVisitor node lookupTable =
    case Node.value node of
        Expression.IfBlock _ thenExpression elseExpression ->
            ( errorsForIf node thenExpression elseExpression lookupTable, lookupTable )

        _ ->
            ( [], lookupTable )


errorsForIf : Node Expression -> Node Expression -> Node Expression -> ModuleNameLookupTable -> List (Error {})
errorsForIf parent thenExpression elseExpression lookupTable =
    case ( Helper.maybeBoolLiteralOfExpression thenExpression lookupTable, Helper.maybeBoolLiteralOfExpression elseExpression lookupTable ) of
        ( Just True, Nothing ) ->
            [ error parent "||" ]

        ( Just False, Nothing ) ->
            [ error parent "&&" ]

        ( Nothing, Just True ) ->
            [ error parent "||" ]

        ( Nothing, Just False ) ->
            [ error parent "&&" ]

        _ ->
            []


error : Node Expression -> String -> Error {}
error node operator =
    Rule.error
        { message = "Use boolean expression instead of if"
        , details =
            [ "When exactly one branch in an if-Block returns a boolean value, you should write a boolean expression instead"
            , "In this case I suggest a simple expression using \"" ++ operator ++ "\""
            ]
        }
        (Node.range node)
