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
        Expression.IfBlock _ statement elseStatement ->
            ( errorsForIf node statement elseStatement lookupTable, lookupTable )

        _ ->
            ( [], lookupTable )


errorsForIf : Node Expression -> Node Expression -> Node Expression -> ModuleNameLookupTable -> List (Error {})
errorsForIf parent statement elseStatement lookupTable =
    case ( Helper.maybeBoolLiteralOfExpression statement lookupTable, Helper.maybeBoolLiteralOfExpression elseStatement lookupTable ) of
        ( Just True, Nothing ) ->
            [ error parent "condition || elseStatement" ]

        ( Just False, Nothing ) ->
            [ error parent "not condition && elseStatement" ]

        ( Nothing, Just True ) ->
            [ error parent "not condition || statement" ]

        ( Nothing, Just False ) ->
            [ error parent "condition && statement" ]

        _ ->
            []


error : Node Expression -> String -> Error {}
error node transformed =
    Rule.error
        { message = "Use boolean expression instead of if"
        , details =
            [ "When either the then path or the else path return a boolean value, you should write a boolean expression"
            , "Instead of \"if condition then statement else elseStatement\""
            , "It can be rewritten as \"" ++ transformed ++ "\""
            ]
        }
        (Node.range node)
