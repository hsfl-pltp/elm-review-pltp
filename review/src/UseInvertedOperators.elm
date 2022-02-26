module UseInvertedOperators exposing (rule)

{-| Forbids the use of not in case of non-boolean operators

@docs rule

-}

import Elm.Pretty
import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Pretty
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
        Expression.OperatorApplication operator _ left right ->
            case inverseExpression operator left right of
                Just inversedExpression ->
                    [ notError parent (Pretty.pretty 120 (Elm.Pretty.prettyExpression inversedExpression)) ]

                Nothing ->
                    []

        _ ->
            []


inverseExpression : String -> Node Expression -> Node Expression -> Maybe Expression
inverseExpression operator left right =
    case transformOperator operator of
        Just transformedOperator ->
            Just (Expression.OperatorApplication transformedOperator Non left right)

        Nothing ->
            Nothing


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


notError : Node Expression -> String -> Error {}
notError node transformed =
    Rule.error
        { message = "Use inverted operator instead of not."
        , details =
            [ "In this case you should use another operator."
            , "It can be rewritten as \"" ++ transformed ++ "\""
            ]
        }
        (Node.range node)
