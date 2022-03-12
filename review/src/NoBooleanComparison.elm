module NoBooleanComparison exposing (rule)

{-| Forbids the comparison with booleans

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Helper
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the comparison with booleans

    config =
        [ NoBooleanComparison.rule
        ]


## Fail

    if bar == True then
        ...
    else
        ...

    if bar == False then
        ...
    else
        ...


## Success

    if bar then
        ...
    else
        ...

-}
initialContext : Rule.ContextCreator () ModuleNameLookupTable
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> lookupTable)
        |> Rule.withModuleNameLookupTable


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoBooleanComparison" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> ModuleNameLookupTable -> ( List (Error {}), ModuleNameLookupTable )
expressionVisitor node lookupTable =
    case Node.value node of
        Expression.OperatorApplication "==" _ left right ->
            ( errorsForOperator node left right lookupTable, lookupTable )

        Expression.OperatorApplication "/=" _ left right ->
            ( errorsForOperator node left right lookupTable, lookupTable )

        _ ->
            ( [], lookupTable )


errorsForOperator : Node Expression -> Node Expression -> Node Expression -> ModuleNameLookupTable -> List (Error {})
errorsForOperator parent left right lookupTable =
    if Helper.isBoolExpression left lookupTable && not (Helper.isBoolExpression right lookupTable) then
        [ ruleError parent ]

    else if not (Helper.isBoolExpression left lookupTable) && Helper.isBoolExpression right lookupTable then
        [ ruleError parent ]

    else
        []


ruleError : Node Expression -> Error {}
ruleError node =
    Rule.error
        { message = "Comparison with boolean literal detected"
        , details =
            [ "There is no need to compare a value of type `Boolean` with `True` or `False`. For example, `if b == True then .. else ..` is the same as `if b then ... else ...`."
            ]
        }
        (Node.range node)
