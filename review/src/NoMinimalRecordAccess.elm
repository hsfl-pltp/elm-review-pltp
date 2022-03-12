module NoMinimalRecordAccess exposing
    ( rule
    , Config
    )

{-| Forbids the use of a record, when only a few components from the record is used

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import List.Extra
import Review.Rule as Rule exposing (Error, Rule)


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
type alias Config =
    { threshold : Int
    , ignoreFunctions : List String
    }


rule : Config -> Rule
rule config =
    Rule.newModuleRuleSchema "NoMinimalRecordAccess" ()
        |> Rule.withSimpleDeclarationVisitor (declarationVisitor config)
        |> Rule.fromModuleRuleSchema


declarationVisitor : Config -> Node Declaration -> List (Error {})
declarationVisitor config node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            errorsForDeclaration config (Node.value declaration)

        _ ->
            []


errorsForDeclaration : Config -> FunctionImplementation -> List (Error {})
errorsForDeclaration config { arguments, expression, name } =
    if List.member (Node.value name) config.ignoreFunctions then
        []

    else
        errorsForArguments config.threshold arguments
            ++ errorsForExpession config.threshold expression


errorsForExpession : Int -> Node Expression -> List (Error {})
errorsForExpession threshold node =
    if List.length (used node) >= threshold then
        [ ruleError threshold node ]

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
        Expression.RecordAccess expr (Node _ name) ->
            [ toRecordString (Node.value expr) name ]

        Expression.Application expressions ->
            List.concatMap used expressions

        Expression.OperatorApplication _ _ left right ->
            List.concatMap used [ left, right ]

        Expression.IfBlock expr left right ->
            List.concatMap used [ expr, left, right ]

        Expression.CaseExpression { expression, cases } ->
            List.concatMap used (expression :: List.map Tuple.second cases)

        Expression.ListExpr expressions ->
            List.concatMap used expressions

        _ ->
            []


toRecordString : Expression -> String -> String
toRecordString expression name =
    case expression of
        Expression.FunctionOrValue [] record ->
            record ++ "." ++ name

        _ ->
            name


errorsForArguments : Int -> List (Node Pattern) -> List (Error {})
errorsForArguments threshold patterns =
    patterns
        |> List.filter (invalidPattern threshold)
        |> List.map (ruleError threshold)


invalidPattern : Int -> Node Pattern -> Bool
invalidPattern threshold node =
    case Node.value node of
        Pattern.RecordPattern components ->
            List.length components <= threshold

        _ ->
            False


ruleError : Int -> Node a -> Error {}
ruleError threshold node =
    Rule.error
        { message = "Unnecessary complex record argument detected."
        , details =
            [ "If a function receives a record as an argument and the function uses " ++ String.fromInt threshold ++ " or less fields from a record, then you should pass the fields as individual arguments to the function."
            ]
        }
        (Node.range node)
