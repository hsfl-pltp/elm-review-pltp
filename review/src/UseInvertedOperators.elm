module UseInvertedOperators exposing (rule)

{-| Forbids the use of not in case of non-boolean operators

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when the use of not

    config =
        [ UseInvertedOperators.rule
        ]


## Fail

    not (a == b)

    not (a /= b)

    not (a > b)

    not (a >= b)

    not (a < b)

    not (a <= b)


## Success

    a /= b

    a == b

    a <= b

    a < b

    a >= b

    a > b

-}
initialContext : Rule.ContextCreator () ModuleNameLookupTable
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> lookupTable)
        |> Rule.withModuleNameLookupTable


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "UseInvertedOperators" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> ModuleNameLookupTable -> ( List (Error {}), ModuleNameLookupTable )
expressionVisitor node lookupTable =
    case Node.value node of
        Expression.Application nodes ->
            ( errorsForApplication node lookupTable nodes, lookupTable )

        _ ->
            ( [], lookupTable )


errorsForApplication : Node Expression -> ModuleNameLookupTable -> List (Node Expression) -> List (Error {})
errorsForApplication parent lookupTable list =
    case list of
        x :: expression :: xs ->
            if isNot x lookupTable then
                errorsForNot parent (Node.value expression)

            else
                errorsForApplication parent lookupTable xs

        _ ->
            []


isNot : Node Expression -> ModuleNameLookupTable -> Bool
isNot node lookupTable =
    case Node.value node of
        Expression.FunctionOrValue _ "not" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Basics" ] ->
                    True

                _ ->
                    False

        _ ->
            False


errorsForNot : Node Expression -> Expression -> List (Error {})
errorsForNot parent expression =
    case expression of
        Expression.ParenthesizedExpression (Node _ expr) ->
            errorsForOperator parent expr

        _ ->
            []


errorsForOperator : Node Expression -> Expression -> List (Error {})
errorsForOperator parent expr =
    case expr of
        Expression.OperatorApplication operator _ _ _ ->
            case transformOperator operator of
                Just transformed ->
                    [ notError parent operator transformed ]

                Nothing ->
                    []

        _ ->
            []


transformOperator : String -> Maybe String
transformOperator operator =
    case operator of
        "==" ->
            Just "/="

        "/=" ->
            Just "=="

        ">" ->
            Just "<="

        "<" ->
            Just ">="

        "<=" ->
            Just ">"

        ">=" ->
            Just "<"

        _ ->
            Nothing


notError : Node Expression -> String -> String -> Error {}
notError node operator transformed =
    Rule.error
        { message = "Use negated operator instead of not"
        , details =
            [ "You can remove the negation and use the negated operator instead. For example, in this case you can use `" ++ transformed ++ "` instead of `" ++ operator ++ "`."
            ]
        }
        (Node.range node)
