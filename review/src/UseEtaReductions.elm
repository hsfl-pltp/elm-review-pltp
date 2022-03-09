module UseEtaReductions exposing
    ( rule
    , ErrorStyle(..)
    )

{-| Forbids function declarations without eta reductions

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, Function, FunctionImplementation)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import List.Extra
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the possibility to apply a eta reduction

    config =
        [ UseEtaReductions.rule
        ]


## Fail

    incList : List Int -> List Int
    incList list =
        map inc list


## Success

    incList : List Int -> List Int
    incList =
        map inc

-}
globalRange : Range
globalRange =
    Range (Location 0 0) (Location 0 0)


type ErrorStyle
    = LocatedError
    | ModuleError


rule : ErrorStyle -> Rule
rule errorStyle =
    Rule.newModuleRuleSchema "UseEtaReductions" ()
        |> Rule.withSimpleExpressionVisitor (expressionVisitor errorStyle)
        |> Rule.withSimpleDeclarationVisitor (declarationVisitor errorStyle)
        |> Rule.fromModuleRuleSchema


expressionVisitor : ErrorStyle -> Node Expression -> List (Error {})
expressionVisitor errorStyle node =
    case Node.value node of
        Expression.LambdaExpression lambda ->
            errorsForLambda errorStyle node lambda

        _ ->
            []


declarationVisitor : ErrorStyle -> Node Declaration -> List (Error {})
declarationVisitor errorStyle node =
    case Node.value node of
        Declaration.FunctionDeclaration fn ->
            errorsForFunction errorStyle fn

        _ ->
            []


errorsForFunction : ErrorStyle -> Function -> List (Error {})
errorsForFunction errorStyle { declaration } =
    errorsFunctionImplementation errorStyle declaration


errorsFunctionImplementation : ErrorStyle -> Node FunctionImplementation -> List (Error {})
errorsFunctionImplementation errorStyle (Node _ { expression, arguments }) =
    case Node.value expression of
        Expression.Application list ->
            errorsForApplication errorStyle expression (List.Extra.last list) (List.Extra.last arguments)

        _ ->
            []


errorsForApplication : ErrorStyle -> Node Expression -> Maybe (Node Expression) -> Maybe (Node Pattern) -> List (Error {})
errorsForApplication errorStyle node expression pattern =
    case ( expression, pattern ) of
        ( Just exp, Just pat ) ->
            if equal exp pat then
                case errorStyle of
                    LocatedError ->
                        [ applicationError (Node.range node) ]

                    ModuleError ->
                        [ applicationError globalRange ]

            else
                []

        _ ->
            []


errorsForLambda : ErrorStyle -> Node Expression -> Expression.Lambda -> List (Error {})
errorsForLambda errorStyle node { expression, args } =
    case List.Extra.last args of
        Nothing ->
            []

        Just arg ->
            if equal expression arg then
                case errorStyle of
                    LocatedError ->
                        [ lambdaError (Node.range node) ]

                    ModuleError ->
                        [ lambdaError globalRange ]

            else
                []


equal : Node Expression -> Node Pattern -> Bool
equal expression pattern =
    case ( Node.value pattern, Node.value expression ) of
        ( Pattern.VarPattern var, Expression.FunctionOrValue [] val ) ->
            var == val

        ( _, Expression.Application expressions ) ->
            case List.Extra.last expressions of
                Nothing ->
                    False

                Just expr ->
                    equal expr pattern

        _ ->
            False


lambdaError : Range -> Error {}
lambdaError range =
    Rule.error
        { message = "Possible eta reduction for labmda detected."
        , details =
            [ "When the last argument of a lambda is the last applied to your epxression, then you should remove both"
            , "Iamgine you have a lambda like \"(\\e -> inc e\", then you can just write \"inc\""
            ]
        }
        range


applicationError : Range -> Error {}
applicationError range =
    Rule.error
        { message = "Possible eta reduction detected."
        , details =
            [ "When the last argument of a function is the last applied to your expression, then you should remove both"
            , "Imagine you have a function with the signature incList : List Int -> List Int, with the implementation \"incList list = List.map inc list\""
            , "When you apply the eta reduction, you can remove the list argument and the last argument of the List.map function : \" incList = List.map inc\""
            ]
        }
        range
