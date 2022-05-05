module UseRecordUpdate exposing (rule)

import AssocList as Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation, RecordSetter)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Rule as Rule exposing (Error, Rule)


type TypeName
    = TypeName String


type VarName
    = VarName String


type RecordField
    = RecordField String


type alias Context =
    { recordAliases : List TypeName
    , accessors : List VarName
    , destructed : List RecordField
    , isFunctionWithRecordReturn : Bool
    }


initialContext : Context
initialContext =
    { recordAliases = []
    , accessors = []
    , destructed = []
    , isFunctionWithRecordReturn = False
    }


rule : Rule
rule =
    Rule.newModuleRuleSchema "UseRecordUpdate" initialContext
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


declarationListVisitor : List (Node Declaration) -> Context -> ( List a, Context )
declarationListVisitor nodes context =
    ( [], { context | recordAliases = List.filterMap customRecord nodes } )


customRecord : Node Declaration -> Maybe TypeName
customRecord node =
    case Node.value node of
        Declaration.AliasDeclaration { name, typeAnnotation } ->
            case Node.value typeAnnotation of
                TypeAnnotation.Record _ ->
                    Just (TypeName (Node.value name))

                _ ->
                    Nothing

        _ ->
            Nothing


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration, signature } ->
            case signature of
                Just sig ->
                    case recordArguments context.recordAliases (.typeAnnotation (Node.value sig)) (.arguments (Node.value declaration)) of
                        Just ( accessors, destructs ) ->
                            Debug.todo "in context aufnehmen"

                        Nothing ->
                            ( [], { context | isFunctionWithRecordReturn = False } )

                Nothing ->
                    ( [], { context | isFunctionWithRecordReturn = False } )

        _ ->
            ( [], { context | isFunctionWithRecordReturn = False } )


recordArguments : List TypeName -> Node TypeAnnotation -> List (Node Pattern) -> Maybe ( List VarName, List RecordField )
recordArguments recordAliases node list =
    let
        recordArgs =
            hasRecordReturnAndRecordArgument recordAliases (typesWithPatterns node list)
    in
    if List.isEmpty recordArgs then
        Nothing

    else
        Just
            ( List.filterMap undestructedRecordName recordArgs
            , List.concatMap destructedRecordField recordArgs
            )


hasRecordReturnAndRecordArgument : List TypeName -> List ( TypeName, Maybe (Node Pattern) ) -> List (Node Pattern)
hasRecordReturnAndRecordArgument recordAliases typesPatterns =
    let
        typeListRev =
            List.reverse (List.map Tuple.first typesPatterns)

        returnMaybe =
            List.head typeListRev

        argumentsMaybe =
            List.tail typeListRev
    in
    case ( returnMaybe, argumentsMaybe ) of
        ( Just return, Just arguments ) ->
            if List.member return recordAliases && List.member return arguments then
                List.filterMap
                    (\( t, p ) ->
                        case p of
                            Just pattern ->
                                if List.member t recordAliases then
                                    Just pattern

                                else
                                    Nothing

                            Nothing ->
                                Nothing
                    )
                    typesPatterns

            else
                []

        _ ->
            []


typesWithPatterns : Node TypeAnnotation -> List (Node Pattern) -> List ( TypeName, Maybe (Node Pattern) )
typesWithPatterns typeAnnotation list =
    case Node.value typeAnnotation of
        TypeAnnotation.FunctionTypeAnnotation head tail ->
            typesWithPatterns head (List.take 1 list) ++ typesWithPatterns tail (List.drop 1 list)

        TypeAnnotation.Typed typed _ ->
            case list of
                [] ->
                    [ ( TypeName (Tuple.second (Node.value typed)), Nothing ) ]

                pattern :: _ ->
                    [ ( TypeName (Tuple.second (Node.value typed)), Just pattern ) ]

        _ ->
            []


undestructedRecordName : Node Pattern -> Maybe VarName
undestructedRecordName node =
    case Node.value node of
        Pattern.VarPattern name ->
            Just (VarName name)

        _ ->
            Nothing


destructedRecordField : Node Pattern -> List RecordField
destructedRecordField node =
    case Node.value node of
        Pattern.RecordPattern list ->
            List.map (\fieldNode -> RecordField (Node.value fieldNode)) list

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
            True

        -- List.member fovName context.destructed && name == fovName
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
