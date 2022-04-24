module UseRecordUpdate exposing (rule)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation, RecordSetter)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "UseRecordUpdate" { isFunctionWithRecordReturn = False, records = [], destructed = [] }
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { isFunctionWithRecordReturn : Bool
    , records : List String
    , destructed : List String
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


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            case function.signature of
                Just sigNode ->
                    let
                        hasToVisit =
                            Node.value sigNode
                                |> .typeAnnotation
                                |> Node.value
                                |> hasRecordReturnAndRecordParameter context

                        destructed =
                            if hasToVisit then
                                destructedRecordFieldNames function.declaration

                            else
                                []
                    in
                    ( [], { context | isFunctionWithRecordReturn = hasToVisit, destructed = destructed } )

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
            List.member return context.records && List.member return parameters

        _ ->
            False


functionTypes : TypeAnnotation -> List String
functionTypes typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.FunctionTypeAnnotation head tail ->
            functionTypes (Node.value head) ++ functionTypes (Node.value tail)

        TypeAnnotation.Typed node _ ->
            [ Tuple.second (Node.value node) ]

        _ ->
            []


destructedRecordFieldNames : Node FunctionImplementation -> List String
destructedRecordFieldNames node =
    Node.value node
        |> .arguments
        |> List.concatMap recordFieldsInPattern


recordFieldsInPattern : Node Pattern -> List String
recordFieldsInPattern node =
    case Node.value node of
        Pattern.RecordPattern list ->
            List.map Node.value list

        _ ->
            []


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    if context.isFunctionWithRecordReturn then
        case Node.value node of
            Expression.RecordExpr list ->
                if List.any (hasUnchangedValue context) list then
                    ( [ ruleError node ], context )

                else
                    ( [], context )

            _ ->
                ( [], context )

    else
        ( [], context )


hasUnchangedValue : Context -> Node RecordSetter -> Bool
hasUnchangedValue context recordSetter =
    let
        ( nameNode, expression ) =
            Node.value recordSetter

        name =
            Node.value nameNode
    in
    case Node.value expression of
        Expression.Application nodes ->
            case List.head nodes of
                Just head ->
                    case Node.value head of
                        Expression.RecordAccessFunction access ->
                            name == access

                        _ ->
                            False

                _ ->
                    False

        Expression.FunctionOrValue _ fovName ->
            List.member fovName context.destructed && name == fovName

        Expression.RecordAccess _ accessNode ->
            name == Node.value accessNode

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
