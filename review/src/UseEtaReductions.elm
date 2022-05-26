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
import Visitor


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


type alias Context =
    { applCount : Int
    , lambdaCount : Int
    }


initialContext : Context
initialContext =
    { applCount = 0, lambdaCount = 0 }


rule : ErrorStyle -> Rule
rule errorStyle =
    case errorStyle of
        LocatedError ->
            Rule.newModuleRuleSchema "UseEtaReductions" ()
                |> Rule.withSimpleExpressionVisitor simpleExpressionVisitor
                |> Rule.withSimpleDeclarationVisitor simpleDeclarationVisitor
                |> Rule.fromModuleRuleSchema

        ModuleError ->
            Rule.newModuleRuleSchema "UseEtaReductions" initialContext
                |> Rule.withExpressionEnterVisitor expressionVisitor
                |> Rule.withDeclarationEnterVisitor declarationVisitor
                |> Rule.withFinalModuleEvaluation finalEvaluation
                |> Rule.fromModuleRuleSchema


simpleExpressionVisitor : Node Expression -> List (Error {})
simpleExpressionVisitor node =
    case Node.value node of
        Expression.LambdaExpression lambda ->
            errorsForLambda node lambda

        _ ->
            []


simpleDeclarationVisitor : Node Declaration -> List (Error {})
simpleDeclarationVisitor node =
    case Node.value node of
        Declaration.FunctionDeclaration fn ->
            errorsFunctionImplementation fn.declaration

        _ ->
            []


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.LambdaExpression { args, expression } ->
            case List.Extra.last args of
                Just arg ->
                    if equal expression arg then
                        ( [], { context | lambdaCount = context.lambdaCount + 1 } )

                    else
                        ( [], context )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                expr =
                    (Node.value declaration).expression

                args =
                    (Node.value declaration).arguments
            in
            case Node.value expr of
                Expression.Application list ->
                    case ( List.Extra.last list, List.Extra.last args ) of
                        ( Just exp, Just pat ) ->
                            if equal exp pat then
                                ( [], { context | applCount = context.applCount + 1 } )

                            else
                                ( [], context )

                        _ ->
                            ( [], context )

                _ ->
                    ( [], context )

        _ ->
            ( [], context )


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    if context.applCount > 0 || context.lambdaCount > 0 then
        [ moduleError context ]

    else
        []


errorsFunctionImplementation : Node FunctionImplementation -> List (Error {})
errorsFunctionImplementation (Node _ { expression, arguments }) =
    case Node.value expression of
        Expression.Application list ->
            errorsForApplication expression (List.Extra.last list) (List.Extra.last arguments)

        _ ->
            []


errorsForApplication : Node Expression -> Maybe (Node Expression) -> Maybe (Node Pattern) -> List (Error {})
errorsForApplication node expression pattern =
    case ( expression, pattern ) of
        ( Just exp, Just pat ) ->
            if equal exp pat then
                [ applicationError node ]

            else
                []

        _ ->
            []


errorsForLambda : Node Expression -> Expression.Lambda -> List (Error {})
errorsForLambda node { expression, args } =
    case List.Extra.last args of
        Nothing ->
            []

        Just arg ->
            if equal expression arg then
                [ lambdaError node ]

            else
                []


equal : Node Expression -> Node Pattern -> Bool
equal expression pattern =
    case ( Node.value pattern, Node.value expression ) of
        ( Pattern.VarPattern var, Expression.FunctionOrValue [] val ) ->
            var == val

        ( Pattern.VarPattern var, Expression.Application expressions ) ->
            case List.Extra.last expressions of
                Nothing ->
                    False

                Just expr ->
                    equal expr pattern && List.Extra.count ((==) var) (Visitor.variableNamesInExpression expression) == 1

        _ ->
            False


lambdaError : Node Expression -> Error {}
lambdaError node =
    Rule.error
        { message = "Possible eta reduction for lambda expression detected"
        , details =
            [ "If the final argument of a lambda expression is the final argument applied to the expression on the right-hand side of the lambda expression, then you can remove both. For example, imagine you have a lambda expression like `\\x -> inc x`, then you can just use `inc` instead."
            ]
        }
        (Node.range node)


applicationError : Node Expression -> Error {}
applicationError node =
    Rule.error
        { message = "Possible eta reduction detected"
        , details =
            [ "If the final argument of a function definition is the final argument applied to the expression on the right-hand side of the function definition, then you can remove both. For example, imagine you have a function with the signature `incList : List Int -> List Int` and the definition `incList list = List.map inc list`. If you apply an eta reduction, you can remove the list argument and the final argument of `List.map` and get `incList = List.map inc`"
            ]
        }
        (Node.range node)


moduleError : Context -> Error {}
moduleError context =
    Rule.error
        { message = "Possible eta reduction detected"
        , details =
            (if context.applCount > 0 then
                [ "I found " ++ String.fromInt context.applCount ++ " function(s) with applicable eta reduction" ]

             else
                []
            )
                ++ (if context.lambdaCount > 0 then
                        [ "I found " ++ String.fromInt context.lambdaCount ++ " lambda expression(s) with applicable eta reduction" ]

                    else
                        []
                   )
        }
        globalRange
