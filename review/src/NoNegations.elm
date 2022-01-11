module NoNegations exposing (rule)

{-| Forbids the use of not ()

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when the use of not

    config =
        [ NoIfNegations.rule
        ]


## Fail

    not (a && b)

    not (a || b)

    not (a == b)

    not (a /= b)

    not (a > b)

    not (a >= b)

    not (a < b)

    not (a <= b)


## Success

    not a || not b

    not a && not b

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
    Rule.newModuleRuleSchemaUsingContextCreator "NoNegations" initialContext
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
            [ notError parent (String.join " " (negateOperator operator left right)) ]

        _ ->
            []


negateOperator : String -> Node Expression -> Node Expression -> List String
negateOperator operator left right =
    if operator == "||" || operator == "&&" then
        case ( Node.value left, Node.value right ) of
            ( Expression.FunctionOrValue [] leftName, Expression.FunctionOrValue [] rightName ) ->
                [ "not", leftName, transformOperator operator, "not", rightName ]

            ( Expression.FunctionOrValue [] leftName, _ ) ->
                [ "not", leftName, transformOperator operator, transform right ]

            ( _, Expression.FunctionOrValue [] rightName ) ->
                [ transform left, transformOperator operator, "not", rightName ]

            _ ->
                [ transform left, transformOperator operator, transform right ]

    else
        [ transform left, transformOperator operator, transform right ]


transform : Node Expression -> String
transform expression =
    case Node.value expression of
        Expression.OperatorApplication operator _ left right ->
            String.join " " (negateOperator operator left right)

        Expression.ParenthesizedExpression expr ->
            "(" ++ transform expr ++ ")"

        Expression.RecordAccess expr (Node _ name) ->
            transform expr ++ "." ++ name

        Expression.FunctionOrValue [] name ->
            name

        Expression.Integer int ->
            String.fromInt int

        Expression.Floatable float ->
            String.fromFloat float

        Expression.Literal string ->
            string

        _ ->
            ""


transformOperator : String -> String
transformOperator operator =
    case operator of
        "&&" ->
            "||"

        "||" ->
            "&&"

        "==" ->
            "/="

        "/=" ->
            "=="

        ">" ->
            "<="

        "<" ->
            ">="

        "<=" ->
            ">"

        ">=" ->
            "<"

        _ ->
            operator


notError : Node Expression -> String -> Error {}
notError node transformed =
    Rule.error
        { message = "Apply De Morgan's laws."
        , details =
            [ "When you apply De Morgan's laws, you dont need \"not\"."
            , "It can be rewritten as \"" ++ transformed ++ "\""
            ]
        }
        (Node.range node)
