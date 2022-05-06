module UseRecordUpdate exposing (rule)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation, RecordSetter)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
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
    , varNames : List VarName
    , recordFields : List RecordField
    , isFunctionWithRecordReturn : Bool
    }


initialContext : Context
initialContext =
    { recordAliases = []
    , varNames = []
    , recordFields = []
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
                        Just ( varNames, recordFields ) ->
                            ( [], { context | isFunctionWithRecordReturn = True, varNames = varNames, recordFields = recordFields } )

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
            argumentsOfRecordReturnType recordAliases (typesWithPatterns node list)
    in
    if List.isEmpty recordArgs then
        Nothing

    else
        Just
            ( List.filterMap undestructedRecordName recordArgs
            , List.concatMap destructedRecordFields recordArgs
            )


argumentsOfRecordReturnType : List TypeName -> List ( TypeName, Maybe (Node Pattern) ) -> List (Node Pattern)
argumentsOfRecordReturnType recordAliases typesPatterns =
    let
        returnMaybe =
            typesPatterns
                |> List.map Tuple.first
                |> List.reverse
                |> List.head
    in
    case returnMaybe of
        Just return ->
            if List.member return recordAliases then
                List.filterMap
                    (\( t, p ) ->
                        case p of
                            Just pattern ->
                                if t == return then
                                    Just pattern

                                else
                                    Nothing

                            Nothing ->
                                Nothing
                    )
                    typesPatterns

            else
                []

        Nothing ->
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


destructedRecordFields : Node Pattern -> List RecordField
destructedRecordFields node =
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
                ( List.filterMap (hasUnchangedValue context.varNames context.recordFields) list, context )

            _ ->
                ( [], context )

    else
        ( [], context )


hasUnchangedValue : List VarName -> List RecordField -> Node RecordSetter -> Maybe (Error {})
hasUnchangedValue varNames recordFields recordSetter =
    let
        ( fieldNode, expression ) =
            Node.value recordSetter

        component =
            Node.value fieldNode

        argumentStrings =
            List.map (\(VarName name) -> name) varNames

        accessorStrings =
            List.map (\(RecordField field) -> field) recordFields
    in
    case Node.value expression of
        Expression.FunctionOrValue _ fovName ->
            if List.member fovName accessorStrings && component == fovName then
                Just (ruleError (Node.range expression))

            else
                Nothing

        Expression.RecordAccess exprNode accessNode ->
            case Node.value exprNode of
                Expression.FunctionOrValue _ var ->
                    if List.member var argumentStrings && component == Node.value accessNode then
                        Just (ruleError (Node.range accessNode))

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


ruleError : Range -> Error {}
ruleError range =
    Rule.error
        { message = "Use Record-Update Syntax"
        , details =
            [ "Record-Expression with unchanging fields detected"
            ]
        }
        range
