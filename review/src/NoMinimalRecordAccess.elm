module NoMinimalRecordAccess exposing (rule)

{-| Forbids the use of a record, when only a few components from the record is used

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import List.Extra
import Review.Rule as Rule exposing (Error, Rule)
import Set


{-| Reports the use of records , where only a few components are used.

    config =
        [ NoMinimalRecordDestructing.rule 1
        ]


## Fail

    type alias Person = {age : Int, name : String}

    viewAge { age } =
        ...

    viewAge person =


## Success

    viewAge age =
        ...

-}
rule : Int -> Rule
rule threshold =
    Rule.newModuleRuleSchema "NoMinimalRecordAccess" ()
        |> Rule.withSimpleDeclarationVisitor (declarationVisitor threshold)
        |> Rule.fromModuleRuleSchema


declarationVisitor : Int -> Node Declaration -> List (Error {})
declarationVisitor threshold node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            errorsForDeclaration threshold (Node.value declaration)

        _ ->
            []


errorsForDeclaration : Int -> FunctionImplementation -> List (Error {})
errorsForDeclaration threshold { arguments, expression } =
    errorsForArguments threshold arguments
        ++ errorsForExpession threshold expression


errorsForExpession : Int -> Node Expression -> List (Error {})
errorsForExpession threshold node =
    if List.length (used node) >= threshold then
        [ accessError threshold node ]

    else
        []


used : Node Expression -> List String
used node =
    node
        |> accessed
        |> List.Extra.unique


accessed : Node Expression -> List String
accessed node =
    case Node.value node of
        Expression.RecordAccess _ (Node _ name) ->
            [ name ]

        Expression.RecordAccessFunction name ->
            [ name ]

        Expression.Application expressions ->
            List.concatMap used expressions

        Expression.OperatorApplication _ _ left right ->
            List.concatMap used [ left, right ]

        Expression.IfBlock expr left right ->
            List.concatMap used [ expr, left, right ]

        Expression.CaseExpression { expression, cases } ->
            List.concatMap used (expression :: List.map Tuple.second cases)

        _ ->
            []


errorsForArguments : Int -> List (Node Pattern) -> List (Error {})
errorsForArguments threshold patterns =
    patterns
        |> List.filter (validRecordPattern threshold)
        |> List.map (destructingError threshold)


validRecordPattern : Int -> Node Pattern -> Bool
validRecordPattern threshold node =
    case Node.value node of
        Pattern.RecordPattern components ->
            List.length components <= threshold

        _ ->
            False


destructingError : Int -> Node Pattern -> Error {}
destructingError threshold node =
    Rule.error
        { message = "To few components used from record destructing"
        , details =
            [ "If you destruct less then " ++ String.fromInt threshold ++ " components from a record, then you sould give the components as parameters to you function"
            , "For example, the record {name: String, age: Int}. The components name and age should be given as single arguements to your function"
            ]
        }
        (Node.range node)


accessError : Int -> Node Expression -> Error {}
accessError threshold node =
    Rule.error
        { message = "To few compoents used in record access"
        , details = [ "If you use less then " ++ String.fromInt threshold ++ " components from record, via record access, you should move give the components as arguments to the function" ]
        }
        (Node.range node)
