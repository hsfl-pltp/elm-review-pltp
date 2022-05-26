module UseCommutingConversions exposing (rule)

{-| Forbids the use of commuting conversions in if and case expressions

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import List.Extra
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of commuting conversions in if and case expressions

    config =
        [ UseCommutingConversionsIf.rule
        ]


## Fail

    a =
        if a then
            f b

        else
            f c


## Success

    a =
        f
            (if a then
                b

             else
                c
            )

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "UseCommutingConversions" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.IfBlock _ left right ->
            errorsForIf node left right

        Expression.CaseExpression { cases } ->
            if List.length cases > 1 then
                errorsForCase node cases

            else
                []

        _ ->
            []


equal : Node Expression -> Node Expression -> Bool
equal left right =
    case ( Node.value left, Node.value right ) of
        ( Expression.FunctionOrValue leftModuleName leftFunc, Expression.FunctionOrValue rightModuleName rightFunc ) ->
            leftModuleName == rightModuleName && leftFunc == rightFunc

        ( Expression.Application leftNodes, Expression.Application rightNodes ) ->
            oneDiff leftNodes rightNodes

        ( Expression.IfBlock leftCond leftThen leftElse, Expression.IfBlock rightCond rightThen rightElse ) ->
            oneDiff [ leftCond, leftThen, leftElse ] [ rightCond, rightThen, rightElse ]

        ( Expression.CaseExpression leftBlock, Expression.CaseExpression rightBlock ) ->
            oneDiff (List.map Tuple.second leftBlock.cases) (List.map Tuple.second rightBlock.cases)

        _ ->
            False


oneDiff : List (Node Expression) -> List (Node Expression) -> Bool
oneDiff leftNodes rightNodes =
    case List.filter not (List.map2 equal leftNodes rightNodes) of
        [ _ ] ->
            True

        _ ->
            False


haveCommonContext : Node Expression -> Node Expression -> Bool
haveCommonContext (Node _ left) (Node _ right) =
    case ( left, right ) of
        ( Expression.Application leftNodes, Expression.Application rightNodes ) ->
            oneDiff leftNodes rightNodes

        _ ->
            False


errorsForIf : Node Expression -> Node Expression -> Node Expression -> List (Error {})
errorsForIf node left right =
    if haveCommonContext left right then
        [ ifError node ]

    else
        []


errorsForCase : Node Expression -> List Expression.Case -> List (Error {})
errorsForCase node cases =
    let
        expressions =
            List.map Tuple.second cases
    in
    if List.all (\( a, b ) -> haveCommonContext a b) (List.Extra.uniquePairs expressions) then
        [ caseErrors node ]

    else
        []


ifError : Node Expression -> Error {}
ifError node =
    Rule.error
        { message = "Possible commuting conversion for if detected"
        , details =
            [ "An expression of the form `if a then f b else f c` can be written as `f (if a then b else c)`"
            ]
        }
        (Node.range node)


caseErrors : Node Expression -> Error {}
caseErrors node =
    Rule.error
        { message = "Possible commuting conversion for case detected"
        , details =
            [ "An expression of the form `case e of p1 -> f e1 ... pn -> f en` can be written as `f (case e of p1 -> e1 ... pn -> en)`"
            ]
        }
        (Node.range node)
