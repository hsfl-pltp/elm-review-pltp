module Visitor exposing (countAppearancesOfString)

import Elm.Syntax.Expression as Expression exposing (Expression, LetDeclaration)
import Elm.Syntax.Node as Node exposing (Node)


countAppearancesOfString : String -> Node Expression -> Int
countAppearancesOfString string node =
    case Node.value node of
        Expression.Application nodes ->
            List.foldl (+) 0 (List.map (countAppearancesOfString string) nodes)

        Expression.OperatorApplication _ _ left right ->
            countAppearancesOfString string left
                + countAppearancesOfString string right

        Expression.FunctionOrValue _ value ->
            if value == string then
                1

            else
                0

        Expression.IfBlock condition hit miss ->
            countAppearancesOfString string condition
                + countAppearancesOfString string hit
                + countAppearancesOfString string miss

        Expression.Negation negated ->
            countAppearancesOfString string negated

        Expression.TupledExpression nodes ->
            List.foldl (+) 0 (List.map (countAppearancesOfString string) nodes)

        Expression.ParenthesizedExpression parenthesized ->
            countAppearancesOfString string parenthesized

        Expression.LetExpression { declarations, expression } ->
            List.foldl
                (+)
                (countAppearancesOfString string expression)
                (List.map
                    (\letDeclaration ->
                        case Node.value letDeclaration of
                            Expression.LetFunction { declaration } ->
                                countAppearancesOfString string (.expression (Node.value declaration))

                            Expression.LetDestructuring _ expr ->
                                countAppearancesOfString string expr
                    )
                    declarations
                )

        Expression.CaseExpression { expression, cases } ->
            List.foldl
                (+)
                (countAppearancesOfString string expression)
                (List.map
                    (\( _, expr ) ->
                        countAppearancesOfString string expr
                    )
                    cases
                )

        Expression.LambdaExpression { expression } ->
            countAppearancesOfString string expression

        Expression.RecordExpr nodes ->
            List.foldl (+)
                0
                (List.map
                    (\recordSetter ->
                        countAppearancesOfString string (Tuple.second (Node.value recordSetter))
                    )
                    nodes
                )

        Expression.ListExpr nodes ->
            List.foldl (+) 0 (List.map (countAppearancesOfString string) nodes)

        Expression.RecordAccess access _ ->
            countAppearancesOfString string access

        Expression.RecordUpdateExpression _ nodes ->
            List.foldl (+)
                0
                (List.map
                    (\recordSetter ->
                        countAppearancesOfString string (Tuple.second (Node.value recordSetter))
                    )
                    nodes
                )

        _ ->
            0
