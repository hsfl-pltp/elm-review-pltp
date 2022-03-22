module NoForbiddenFeatures exposing
    ( rule
    , Config
    )

{-| Forbids the use of functions and features, defined by configuration

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type exposing (ValueConstructor)
import Helper
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of forbidden features and functions

    config =
        [ NoForbiddenFeatures.rule
            { operators = [ "|>", "<|" ]
            , functions = [ "List.map", "List.foldr" ]
            , letIn = True
            , productDataTypes = False
            }
        ]


## Example for operators


### Fail

    foo bar =
        bar |> func


### Success

    foo bar =
        func bar


## Example for functions

    foo bar =
        List.map func bar

    foo bar =
        case bar of
            ...
            ...

-}
type alias Config =
    { operators : List String
    , functions : List String
    , letIn : Bool
    , algebraicDataTypes : Bool
    , lambda : Bool
    }



-- Context


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , config : Config
    }


initialContext : Config -> Rule.ContextCreator () Context
initialContext config =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , config = config
            }
        )
        |> Rule.withModuleNameLookupTable


rule : Config -> Rule
rule config =
    Rule.newModuleRuleSchemaUsingContextCreator "NoForbiddenFeatures" (initialContext config)
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema



-- Declaration visitor


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor node context =
    if context.config.algebraicDataTypes then
        ( errorsForDeclaration node, context )

    else
        ( [], context )


errorsForDeclaration : Node Declaration -> List (Error {})
errorsForDeclaration node =
    case Node.value node of
        Declaration.CustomTypeDeclaration { constructors } ->
            constructors
                |> List.filter hasValueConstructor
                |> List.map (ruleError "algebraic data types")

        _ ->
            []


hasValueConstructor : Node ValueConstructor -> Bool
hasValueConstructor (Node _ { arguments }) =
    not (List.isEmpty arguments)



-- Expression visitor


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.OperatorApplication operator _ _ _ ->
            ( errorsForFeature context.config.operators operator node, context )

        Expression.FunctionOrValue _ func ->
            ( errorsForFeature context.config.functions (Helper.functionName node context.lookupTable func) node, context )

        Expression.LetExpression _ ->
            ( errorsForLetIn context.config.letIn node, context )

        Expression.LambdaExpression _ ->
            ( toErrorsIfForbidden context.config.lambda (ruleError "lambda expressions") node, context )

        _ ->
            ( [], context )


errorsForFeature : List String -> String -> Node Expression -> List (Error {})
errorsForFeature forbidden feature node =
    toErrorsIfForbidden (List.member feature forbidden) (ruleError feature) node


errorsForLetIn : Bool -> Node Expression -> List (Error {})
errorsForLetIn isEnabled =
    toErrorsIfForbidden isEnabled (ruleError "let ... in ...")


toErrorsIfForbidden : Bool -> (Node a -> Error {}) -> Node a -> List (Error {})
toErrorsIfForbidden isForbidden toError node =
    if isForbidden then
        [ toError node ]

    else
        []


ruleError : String -> Node a -> Error {}
ruleError feature node =
    Rule.error
        { message = "Advanced feature detected"
        , details =
            [ "The use of " ++ feature ++ " is not allowed. You have to solve the problem in another way..."
            , "Find out why: https://hsfl-pltp.github.io/elm-review-pltp/"
            ]
        }
        (Node.range node)
