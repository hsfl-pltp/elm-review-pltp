module NoMinimalRecordAccess exposing (rule)

import AssocList as Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
import Review.Rule as Rule exposing (Error, Rule)


type TypeName
    = TypeName String


type FunctionScope
    = FunctionScope Range


type VarName
    = VarName String


type RecordField
    = RecordField String


type alias Context =
    { recordAliases : Dict TypeName Int
    , usedRecords : Dict FunctionScope (Dict VarName ( TypeName, Dict RecordField () ))
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


customRecords : Node Declaration -> Maybe ( TypeName, Int )
customRecords node =
    case Node.value node of
        Declaration.AliasDeclaration { name, typeAnnotation } ->
            case Node.value typeAnnotation of
                TypeAnnotation.Record recDef ->
                    Just ( TypeName (Node.value name), List.length recDef )

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
                                                ( recType, Dict.empty )
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


recordParameters : Dict TypeName Int -> Node TypeAnnotation -> List (Node Pattern) -> ( List ( VarName, TypeName ), List (Error {}) )
recordParameters recordAliases node list =
    let
        typesPatterns =
            typesWithPatterns node list
    in
    ( List.filterMap (undestructedRecordType recordAliases) typesPatterns
    , List.filterMap (destructedRecordPattern recordAliases) typesPatterns
    )


typesWithPatterns : Node TypeAnnotation -> List (Node Pattern) -> List ( TypeName, Node Pattern )
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
                    [ ( TypeName (Tuple.second (Node.value typed)), pattern ) ]

        _ ->
            []


undestructedRecordType : Dict TypeName Int -> ( TypeName, Node Pattern ) -> Maybe ( VarName, TypeName )
undestructedRecordType moduleTypeNames ( typeName, node ) =
    if Dict.member typeName moduleTypeNames then
        case Node.value node of
            Pattern.VarPattern name ->
                Just ( VarName name, typeName )

            _ ->
                Nothing

    else
        Nothing


destructedRecordPattern : Dict TypeName Int -> ( TypeName, Node Pattern ) -> Maybe (Error {})
destructedRecordPattern moduleTypeNames ( typeName, node ) =
    case Node.value node of
        Pattern.RecordPattern list ->
            case Dict.get typeName moduleTypeNames of
                Just numberOfFields ->
                    if numberOfFields == List.length list then
                        Nothing

                    else
                        Just (destructedError node typeName (List.length list) numberOfFields)

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


updateUsedRecords : Dict FunctionScope (Dict VarName ( TypeName, Dict RecordField () )) -> FunctionScope -> Expression -> String -> Dict FunctionScope (Dict VarName ( TypeName, Dict RecordField () ))
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
                                            Tuple.mapSecond (\dict -> Dict.insert (RecordField field) () dict) tuple
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


functionToErrorList : Dict TypeName Int -> FunctionScope -> Dict VarName ( TypeName, Dict RecordField () ) -> List (Error {}) -> List (Error {})
functionToErrorList recordAliases functionScope recordsInFunction errorList =
    List.filterMap (recordToError functionScope recordAliases) (Dict.toList recordsInFunction) ++ errorList


recordToError : FunctionScope -> Dict TypeName Int -> ( a, ( TypeName, Dict b () ) ) -> Maybe (Error {})
recordToError functionScope recordFieldsCount recordFieldUsed =
    let
        recordAlias =
            Tuple.first (Tuple.second recordFieldUsed)

        used =
            Dict.size (Tuple.second (Tuple.second recordFieldUsed))
    in
    Dict.get recordAlias recordFieldsCount
        |> Maybe.andThen
            (\fieldCount ->
                if fieldCount == used then
                    Nothing

                else
                    Just (accessError functionScope recordAlias used fieldCount)
            )


destructedError : Node a -> TypeName -> Int -> Int -> Error {}
destructedError node (TypeName record) used all =
    Rule.error
        { message = "Non-exhaustive Record Destructing detected"
        , details =
            [ "You only used " ++ String.fromInt used ++ " of " ++ String.fromInt all ++ " fields from the record: " ++ record
            ]
        }
        (Node.range node)


accessError : FunctionScope -> TypeName -> Int -> Int -> Error {}
accessError (FunctionScope range) (TypeName record) used all =
    Rule.error
        { message = "Non-exhaustive Record Access detected"
        , details =
            [ "You only used " ++ String.fromInt used ++ " of " ++ String.fromInt all ++ " fields from the record: " ++ record
            ]
        }
        range
