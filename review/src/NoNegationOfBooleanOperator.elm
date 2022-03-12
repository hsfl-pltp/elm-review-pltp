module NoNegationOfBooleanOperator exposing (rule)

{-| Forbids the use of not in case of boolean operators

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when the use of not

    config =
        [ NoNegationOfBooleanOperator.rule
        ]


## Fail

    not (a && b)

    not (a || b)


## Success

    not a || not b

    not a && not b

-}
initialContext : Rule.ContextCreator () ModuleNameLookupTable
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> lookupTable)
        |> Rule.withModuleNameLookupTable


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoNegationOfBooleanOperator" initialContext
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
        [ func, arg ] ->
            if isNot func lookupTable then
                errorsForNot parent (Node.value arg)

            else
                []

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
            if operator == "&&" || operator == "||" then
                [ notError parent ]

            else
                []

        _ ->
            []


notError : Node Expression -> Error {}
notError node =
    Rule.error
        { message = "Possibility to apply De Morgan's law detected"
        , details =
            [ "If you apply one of De Morgan's laws, the expression that is negated becomes less complex."
            , "If you don't remember the laws, here are little examples:\n`not (a && b) = not a || not b`\n`not (a || b) = not a && not b`"
            ]
        }
        (Node.range node)
