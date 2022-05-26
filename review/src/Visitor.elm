module Visitor exposing (variableNamesInExpression)

import Elm.Syntax.Expression as Expression exposing (Expression(..), LetDeclaration)
import Elm.Syntax.Node as Node exposing (Node)


variableNamesInExpression : Node Expression -> List String
variableNamesInExpression expr =
    case Node.value expr of
        Application nodes ->
            List.concatMap variableNamesInExpression nodes

        OperatorApplication _ _ left right ->
            variableNamesInExpression left ++ variableNamesInExpression right

        FunctionOrValue _ string ->
            [ string ]

        IfBlock condition hit miss ->
            variableNamesInExpression condition ++ variableNamesInExpression hit ++ variableNamesInExpression miss

        Negation node ->
            variableNamesInExpression node

        TupledExpression nodes ->
            List.concatMap variableNamesInExpression nodes

        ParenthesizedExpression node ->
            variableNamesInExpression node

        LetExpression { declarations, expression } ->
            variableNamesInExpression expression
                ++ List.concatMap
                    (\letDeclaration ->
                        case Node.value letDeclaration of
                            Expression.LetFunction { declaration } ->
                                variableNamesInExpression (.expression (Node.value declaration))

                            Expression.LetDestructuring _ node ->
                                variableNamesInExpression node
                    )
                    declarations

        CaseExpression { expression, cases } ->
            variableNamesInExpression expression
                ++ List.concatMap (\c -> variableNamesInExpression (Tuple.second c)) cases

        LambdaExpression { expression } ->
            variableNamesInExpression expression

        RecordExpr nodes ->
            List.concatMap (\recordSetter -> variableNamesInExpression (Tuple.second (Node.value recordSetter))) nodes

        ListExpr nodes ->
            List.concatMap variableNamesInExpression nodes

        RecordAccess node _ ->
            variableNamesInExpression node

        RecordUpdateExpression _ nodes ->
            List.concatMap (\recordSetter -> variableNamesInExpression (Tuple.second (Node.value recordSetter))) nodes

        _ ->
            []
