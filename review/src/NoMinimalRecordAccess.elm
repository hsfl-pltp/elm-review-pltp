module NoMinimalRecordAccess exposing (rule)

import AssocList as Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
import Ext.UniqueList as Set exposing (Set)
import Review.Rule as Rule exposing (Error, Rule)


type RecordAlias
    = RecordAlias String


type FunctionScope
    = FunctionScope Range


type VarName
    = VarName String


type RecordField
    = RecordField String


type alias Context =
    { recordAliases : Dict RecordAlias Int
    , usedRecords : Dict FunctionScope (Dict VarName ( RecordAlias, Set RecordField ))
    , currentFunction : Maybe FunctionScope
    }


initialContext : Context
initialContext =
    { recordAliases = Dict.empty
    , usedRecords = Dict.empty
    , currentFunction = Nothing
    }


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoMinimalRecordAccess" initialContext
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


declarationListVisitor : List (Node Declaration) -> Context -> ( List (Error {}), Context )
declarationListVisitor declarations context =
    ( []
    , { context
        | recordAliases =
            declarations
                |> List.filterMap customRecords
                |> Dict.fromList
      }
    )


customRecords : Node Declaration -> Maybe ( RecordAlias, Int )
customRecords node =
    case Node.value node of
        Declaration.AliasDeclaration { name, typeAnnotation } ->
            case Node.value typeAnnotation of
                TypeAnnotation.Record recDef ->
                    Just ( RecordAlias (Node.value name), List.length recDef )

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
                    let
                        ( funcTypes, destructedErrors ) =
                            recordParameters context.recordAliases (.typeAnnotation (Node.value sig)) (.arguments (Node.value declaration))

                        dictForRecords =
                            funcTypes
                                |> List.map
                                    (\var ->
                                        Tuple.mapSecond
                                            (\recType ->
                                                ( recType, Set.empty )
                                            )
                                            var
                                    )
                                |> Dict.fromList
                    in
                    ( destructedErrors
                    , { context
                        | usedRecords =
                            Dict.insert (FunctionScope (Node.range node)) dictForRecords context.usedRecords
                        , currentFunction =
                            Just (FunctionScope (Node.range node))
                      }
                    )

                Nothing ->
                    ( [], { context | currentFunction = Nothing } )

        _ ->
            ( [], { context | currentFunction = Nothing } )


recordParameters : Dict RecordAlias Int -> Node TypeAnnotation -> List (Node Pattern) -> ( List ( VarName, RecordAlias ), List (Error {}) )
recordParameters recordAliases node list =
    let
        typesPatterns =
            typesWithPatterns node list
    in
    ( List.filterMap (undestructedRecordType recordAliases) typesPatterns
    , List.filterMap (destructedRecordPattern recordAliases) typesPatterns
    )


typesWithPatterns : Node TypeAnnotation -> List (Node Pattern) -> List ( RecordAlias, Node Pattern )
typesWithPatterns typeAnnotation list =
    case Node.value typeAnnotation of
        TypeAnnotation.FunctionTypeAnnotation head tail ->
            case Node.value tail of
                TypeAnnotation.FunctionTypeAnnotation _ _ ->
                    typesWithPatterns head (List.take 1 list) ++ typesWithPatterns tail (List.drop 1 list)

                _ ->
                    typesWithPatterns head (List.take 1 list)

        TypeAnnotation.Typed typed _ ->
            case list of
                [] ->
                    []

                pattern :: _ ->
                    [ ( RecordAlias (Tuple.second (Node.value typed)), pattern ) ]

        _ ->
            []


undestructedRecordType : Dict RecordAlias Int -> ( RecordAlias, Node Pattern ) -> Maybe ( VarName, RecordAlias )
undestructedRecordType moduleRecordAliases ( recordAlias, node ) =
    if Dict.member recordAlias moduleRecordAliases then
        case Node.value node of
            Pattern.VarPattern name ->
                Just ( VarName name, recordAlias )

            _ ->
                Nothing

    else
        Nothing


destructedRecordPattern : Dict RecordAlias Int -> ( RecordAlias, Node Pattern ) -> Maybe (Error {})
destructedRecordPattern moduleRecordAliases ( recordAlias, node ) =
    case Node.value node of
        Pattern.RecordPattern list ->
            case Dict.get recordAlias moduleRecordAliases of
                Just numberOfFields ->
                    if numberOfFields == List.length list then
                        Nothing

                    else
                        Just (destructedError node recordAlias (List.length list) numberOfFields)

                Nothing ->
                    Nothing

        _ ->
            Nothing


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case context.currentFunction of
        Just functionScope ->
            case Node.value node of
                Expression.RecordAccess recordNode fieldNode ->
                    ( [], { context | usedRecords = updateUsedRecords context.usedRecords functionScope (Node.value recordNode) (Node.value fieldNode) } )

                Expression.Application (fun :: arguments) ->
                    case Node.value fun of
                        Expression.RecordAccessFunction field ->
                            case List.head arguments of
                                Just recordNode ->
                                    ( [], { context | usedRecords = updateUsedRecords context.usedRecords functionScope (Node.value recordNode) field } )

                                _ ->
                                    ( [], context )

                        _ ->
                            ( [], context )

                _ ->
                    ( [], context )

        Nothing ->
            ( [], context )


updateUsedRecords : Dict FunctionScope (Dict VarName ( RecordAlias, Set RecordField )) -> FunctionScope -> Expression -> String -> Dict FunctionScope (Dict VarName ( RecordAlias, Set RecordField ))
updateUsedRecords usedRecords functionScope record field =
    case Dict.get functionScope usedRecords of
        Just recordsInFunction ->
            case record of
                Expression.FunctionOrValue _ recordName ->
                    let
                        updatedRecordsInFunction =
                            Dict.update (VarName recordName)
                                (\recordData ->
                                    Maybe.map
                                        (\tuple ->
                                            Tuple.mapSecond (\set -> Set.insert (RecordField field) set) tuple
                                        )
                                        recordData
                                )
                                recordsInFunction
                    in
                    Dict.update functionScope (\_ -> Just updatedRecordsInFunction) usedRecords

                _ ->
                    usedRecords

        Nothing ->
            usedRecords


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    Dict.foldl (functionToErrorList context.recordAliases) [] context.usedRecords


functionToErrorList : Dict RecordAlias Int -> FunctionScope -> Dict VarName ( RecordAlias, Set RecordField ) -> List (Error {}) -> List (Error {})
functionToErrorList recordAliases functionScope recordsInFunction errorList =
    List.filterMap (recordToError functionScope recordAliases) (Dict.toList recordsInFunction) ++ errorList


recordToError : FunctionScope -> Dict RecordAlias Int -> ( a, ( RecordAlias, Set b ) ) -> Maybe (Error {})
recordToError functionScope recordFieldsCount recordFieldUsed =
    let
        recordAlias =
            Tuple.first (Tuple.second recordFieldUsed)

        used =
            Set.size (Tuple.second (Tuple.second recordFieldUsed))
    in
    Dict.get recordAlias recordFieldsCount
        |> Maybe.andThen
            (\fieldCount ->
                if fieldCount == used then
                    Nothing

                else
                    Just (accessError functionScope recordAlias used fieldCount)
            )


destructedError : Node a -> RecordAlias -> Int -> Int -> Error {}
destructedError node (RecordAlias record) used all =
    Rule.error
        { message = "Non-exhaustive Record Destructing detected"
        , details =
            [ "You only used " ++ String.fromInt used ++ " of " ++ String.fromInt all ++ " fields from the record: " ++ record
            ]
        }
        (Node.range node)


accessError : FunctionScope -> RecordAlias -> Int -> Int -> Error {}
accessError (FunctionScope range) (RecordAlias record) used all =
    Rule.error
        { message = "Non-exhaustive Record Access detected"
        , details =
            [ "You only used " ++ String.fromInt used ++ " of " ++ String.fromInt all ++ " fields from the record: " ++ record
            ]
        }
        range
