module UseEtaReductions exposing
    ( rule
    , ErrorStyle(..)
    )

{-| Forbids function declarations without eta reductions

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, Function, FunctionImplementation)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import List.Extra as List
import Maybe.Extra as Maybe
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
type ErrorStyle
    = LocatedError
    | ModuleError


type alias Context =
    { applCount : Int
    , lambdaCount : Int
    , moduleRange : Range
    }


initialContext : Context
initialContext =
    { applCount = 0, lambdaCount = 0, moduleRange = Range.emptyRange }


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
                |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
                |> Rule.withExpressionEnterVisitor expressionVisitor
                |> Rule.withDeclarationEnterVisitor declarationVisitor
                |> Rule.withFinalModuleEvaluation finalEvaluation
                |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : Node Module -> Context -> ( List (Error {}), Context )
moduleDefinitionVisitor node context =
    ( [], { context | moduleRange = Node.range node } )


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
            case List.last args of
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
                    case ( List.last list, List.last args ) of
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
            errorsForApplication expression (List.last list) (List.last arguments)

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
    case List.last args of
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
            case List.last expressions of
                Nothing ->
                    False

                Just expr ->
                    equal expr pattern && List.count ((==) var) (Visitor.variableNamesInExpression expression) == 1

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
        , details = Maybe.toList (errorMessage "function" context.applCount) ++ Maybe.toList (errorMessage "lambda expression" context.lambdaCount)
        }
        context.moduleRange


errorMessage : String -> Int -> Maybe String
errorMessage entity count =
    let
        plural string n =
            if n == 1 then
                string

            else
                string ++ "s"
    in
    if count > 0 then
        Just ("I found " ++ String.fromInt count ++ " " ++ plural entity count ++ " where eta reduction can be applied.")

    else
        Nothing
