module UseRecordUpdate exposing (rule)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, RecordSetter)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "UseRecordUpdate" { isFunctionWithRecordReturn = False, records = [] }
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { isFunctionWithRecordReturn : Bool
    , records : List String
    }


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor nodes context =
    ( [], { context | records = List.filterMap recordAlias nodes } )


recordAlias : Node Declaration -> Maybe String
recordAlias node =
    case Node.value node of
        Declaration.AliasDeclaration { name, typeAnnotation } ->
            case Node.value typeAnnotation of
                TypeAnnotation.Record _ ->
                    Just (Node.value name)

                _ ->
                    Nothing

        _ ->
            Nothing


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            case function.signature of
                Just (Node _ sigNode) ->
                    ( []
                    , { context
                        | isFunctionWithRecordReturn =
                            sigNode.typeAnnotation
                                |> Node.value
                                |> hasRecordReturnAndRecordParameter context
                      }
                    )

                Nothing ->
                    ( [], { context | isFunctionWithRecordReturn = False } )

        _ ->
            ( [], { context | isFunctionWithRecordReturn = False } )


hasRecordReturnAndRecordParameter : Context -> TypeAnnotation -> Bool
hasRecordReturnAndRecordParameter context typeAnnotation =
    let
        typeListRev =
            functionTypes typeAnnotation
                |> List.reverse

        returnMaybe =
            List.head typeListRev

        parametersMaybe =
            List.tail typeListRev
    in
    case ( returnMaybe, parametersMaybe ) of
        ( Just return, Just parameters ) ->
            isRecord context return && List.member return parameters

        _ ->
            False


functionTypes : TypeAnnotation -> List TypeAnnotation
functionTypes typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.FunctionTypeAnnotation head tail ->
            functionTypes (Node.value head) ++ functionTypes (Node.value tail)

        _ ->
            [ typeAnnotation ]


isRecord : Context -> TypeAnnotation -> Bool
isRecord context typeAnnotation =
    case typeAnnotation of
        Typed node _ ->
            List.member (Tuple.second (Node.value node)) context.records

        _ ->
            False


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    if context.isFunctionWithRecordReturn then
        case Node.value node of
            Expression.RecordExpr list ->
                if List.any hasUnchangedValue list then
                    ( [ ruleError node ], context )

                else
                    ( [], context )

            _ ->
                ( [], context )

    else
        ( [], context )


hasUnchangedValue : Node RecordSetter -> Bool
hasUnchangedValue recordSetter =
    let
        ( name, expression ) =
            Node.value recordSetter
    in
    nameInExpression (Node.value name) expression


nameInExpression : String -> Node Expression -> Bool
nameInExpression name node =
    case Node.value node of
        Expression.Application nodes ->
            List.any (nameInExpression name) nodes

        Expression.ParenthesizedExpression parenthesized ->
            nameInExpression name parenthesized

        Expression.FunctionOrValue _ fovName ->
            name == fovName

        Expression.RecordAccess _ fNode ->
            name == Node.value fNode

        Expression.RecordAccessFunction fName ->
            name == fName

        _ ->
            False


ruleError : Node a -> Error {}
ruleError node =
    Rule.error
        { message = "Use Record-Update Syntax"
        , details =
            [ "Record-Expression with unchanging fields detected"
            ]
        }
        (Node.range node)
