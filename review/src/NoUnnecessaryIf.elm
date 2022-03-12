module NoUnnecessaryIf exposing (rule)

{-| Forbids the use of unnecessary if statements

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Helper
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of unnecessary if statements

    config =
        [ UnnecessaryIf.rule
        ]


## Fail

    foo : Bool -> Bool
    foo bar =
        if bar then
            True

        else
            False

    foo2 : Bool -> Bool
    foo2 bar =
        if bar then
            False

        else
            True


## Success

    foo : Bool -> Bool
    foo bar =
        bar

    foo2 : Bool -> Bool
    foo2 bar =
        not bar

-}
initialContext : Rule.ContextCreator () ModuleNameLookupTable
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> lookupTable)
        |> Rule.withModuleNameLookupTable


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "UnnecessaryIf" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> ModuleNameLookupTable -> ( List (Error {}), ModuleNameLookupTable )
expressionVisitor node lookupTable =
    case Node.value node of
        Expression.IfBlock _ left right ->
            ( errorsForIf node left right lookupTable ++ errorsForIf node right left lookupTable, lookupTable )

        _ ->
            ( [], lookupTable )


errorsForIf : Node Expression -> Node Expression -> Node Expression -> ModuleNameLookupTable -> List (Error {})
errorsForIf parent left right lookupTable =
    case ( Node.value left, Node.value right ) of
        ( Expression.FunctionOrValue _ "True", Expression.FunctionOrValue _ "False" ) ->
            if Helper.isBoolExpression left lookupTable && Helper.isBoolExpression right lookupTable then
                [ ifError parent ]

            else
                []

        _ ->
            []


ifError : Node Expression -> Error {}
ifError node =
    Rule.error
        { message = "Unnecessary if detected"
        , details =
            [ "Instead of an `if` expression that yields `True` and `False` in its branches you can use the boolean expression itself. For example, `if b then True else False` is the same as `b`."
            ]
        }
        (Node.range node)
