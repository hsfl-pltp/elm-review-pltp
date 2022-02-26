module NoNegationOfBooleanOperator exposing (rule)

{-| Forbids the use of not in case of boolean operators

@docs rule

-}

import Elm.Pretty
import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Pretty
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
        Expression.OperatorApplication operator _ left right ->
            case deMorgan operator left right of
                Just transformedExpression ->
                    [ notError parent (Pretty.pretty 120 (Elm.Pretty.prettyExpression transformedExpression)) ]

                Nothing ->
                    []

        _ ->
            []


deMorgan : String -> Node Expression -> Node Expression -> Maybe Expression
deMorgan operator left right =
    case transformOperator operator of
        Just transformedOperator ->
            Just (Expression.OperatorApplication transformedOperator Non (negateExpression left) (negateExpression right))

        Nothing ->
            Nothing



{- Negation of Expression could be improved by adding more cases:
   when parentheses are obsolete
   double negation "not not"
-}


negateExpression : Node Expression -> Node Expression
negateExpression expr =
    case Node.value expr of
        Expression.ParenthesizedExpression _ ->
            pseudoNode (Expression.Application [ pseudoNode (Expression.FunctionOrValue [ "Basics" ] "not"), expr ])

        _ ->
            pseudoNode (Expression.Application [ pseudoNode (Expression.FunctionOrValue [ "Basics" ] "not"), pseudoNode (Expression.ParenthesizedExpression expr) ])


pseudoNode : Expression -> Node Expression
pseudoNode expr =
    Node (Range (Location -1 -1) (Location -1 -1)) expr


transformOperator : String -> Maybe String
transformOperator operator =
    case operator of
        "&&" ->
            Just "||"

        "||" ->
            Just "&&"

        _ ->
            Nothing


notError : Node Expression -> String -> Error {}
notError node transformed =
    Rule.error
        { message = "Apply De Morgan's laws."
        , details =
            [ "When you apply De Morgan's laws, you don't need \"not\"."
            , "It can be rewritten as \"" ++ transformed ++ "\""
            ]
        }
        (Node.range node)
