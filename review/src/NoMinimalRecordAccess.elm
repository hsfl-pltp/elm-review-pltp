module NoMinimalRecordAccess exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


type alias Context =
    { recordAliases : Dict String Int
    , usedRecords : Dict String (Dict String ( String, Set String ))
    , functionRanges : Dict String Range
    , currentFunction : Maybe String
    }


initialContext : Context
initialContext =
    { recordAliases = Dict.empty
    , usedRecords = Dict.empty
    , functionRanges = Dict.empty
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


customRecords : Node Declaration -> Maybe ( String, Int )
customRecords node =
    case Node.value node of
        Declaration.AliasDeclaration { name, typeAnnotation } ->
            case Node.value typeAnnotation of
                TypeAnnotation.Record recDef ->
                    Just ( Node.value name, List.length recDef )

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
                        functionName =
                            declaration
                                |> Node.value
                                |> .name
                                |> Node.value

                        typeAnnotation =
                            sig
                                |> Node.value
                                |> .typeAnnotation

                        patterns =
                            declaration
                                |> Node.value
                                |> .arguments

                        ( funcTypes, destructedErrors ) =
                            recordParameters context.recordAliases typeAnnotation patterns

                        dictForRecords =
                            funcTypes
                                |> List.map (\( n, t ) -> ( n, ( t, Set.empty ) ))
                                |> Dict.fromList
                    in
                    ( destructedErrors
                    , { context
                        | usedRecords =
                            Dict.insert functionName dictForRecords context.usedRecords
                        , functionRanges =
                            Dict.insert functionName (Node.range node) context.functionRanges
                        , currentFunction =
                            Just functionName
                      }
                    )

                Nothing ->
                    ( [], { context | currentFunction = Nothing } )

        _ ->
            ( [], { context | currentFunction = Nothing } )


recordParameters : Dict String Int -> Node TypeAnnotation -> List (Node Pattern) -> ( List ( String, String ), List (Error {}) )
recordParameters recordAliases node list =
    let
        typesPatterns =
            typesWithPatterns node list
    in
    ( List.filterMap (undestructedRecordType recordAliases) typesPatterns
    , List.filterMap (destructedRecordPattern recordAliases) typesPatterns
    )


typesWithPatterns : Node TypeAnnotation -> List (Node Pattern) -> List ( String, Node Pattern )
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
                    [ ( Tuple.second (Node.value typed), pattern ) ]

        _ ->
            []


undestructedRecordType : Dict String Int -> ( String, Node Pattern ) -> Maybe ( String, String )
undestructedRecordType recordAliases ( typeName, node ) =
    if Dict.member typeName recordAliases then
        case Node.value node of
            Pattern.VarPattern name ->
                Just ( name, typeName )

            _ ->
                Nothing

    else
        Nothing


destructedRecordPattern : Dict String Int -> ( String, Node Pattern ) -> Maybe (Error {})
destructedRecordPattern recordAliases ( name, node ) =
    case Node.value node of
        Pattern.RecordPattern list ->
            case Dict.get name recordAliases of
                Just numberOfFields ->
                    if numberOfFields == List.length list then
                        Nothing

                    else
                        Just (destructedError node name (List.length list) numberOfFields)

                Nothing ->
                    Nothing

        _ ->
            Nothing


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case context.currentFunction of
        Just functionName ->
            case Node.value node of
                Expression.RecordAccess recordNode fieldNode ->
                    ( [], { context | usedRecords = updateUsedRecords context.usedRecords functionName (Node.value recordNode) (Node.value fieldNode) } )

                Expression.Application (fun :: arguments) ->
                    case Node.value fun of
                        Expression.RecordAccessFunction field ->
                            case List.head arguments of
                                Just recordNode ->
                                    ( [], { context | usedRecords = updateUsedRecords context.usedRecords functionName (Node.value recordNode) field } )

                                _ ->
                                    ( [], context )

                        _ ->
                            ( [], context )

                _ ->
                    ( [], context )

        Nothing ->
            ( [], context )


updateUsedRecords : Dict String (Dict String ( String, Set String )) -> String -> Expression -> String -> Dict String (Dict String ( String, Set String ))
updateUsedRecords usedRecords functionName record field =
    case Dict.get functionName usedRecords of
        Just recordsInFunction ->
            case record of
                Expression.FunctionOrValue _ recordName ->
                    let
                        updatedRecordsInFunction =
                            Dict.update recordName
                                (\recordData ->
                                    Maybe.map
                                        (\tuple ->
                                            Tuple.mapSecond (\set -> Set.insert field set) tuple
                                        )
                                        recordData
                                )
                                recordsInFunction
                    in
                    Dict.update functionName (\_ -> Just updatedRecordsInFunction) usedRecords

                _ ->
                    usedRecords

        Nothing ->
            usedRecords


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    Dict.foldl (functionToErrorList context.recordAliases context.functionRanges) [] context.usedRecords


functionToErrorList : Dict String Int -> Dict String Range -> String -> Dict String ( String, Set String ) -> List (Error {}) -> List (Error {})
functionToErrorList recordAliases functionRanges functionName recordsInFunction errorList =
    case Dict.get functionName functionRanges of
        Just range ->
            List.filterMap (recordToError range recordAliases) (Dict.toList recordsInFunction) ++ errorList

        Nothing ->
            errorList


recordToError : Range -> Dict String Int -> ( a, ( String, Set a ) ) -> Maybe (Error {})
recordToError range recordFieldsCount recordFieldUsed =
    let
        recordType =
            Tuple.first (Tuple.second recordFieldUsed)

        used =
            Set.size (Tuple.second (Tuple.second recordFieldUsed))
    in
    Dict.get recordType recordFieldsCount
        |> Maybe.andThen
            (\fieldCount ->
                if fieldCount == used then
                    Nothing

                else
                    Just (accessError range recordType used fieldCount)
            )


destructedError : Node a -> String -> Int -> Int -> Error {}
destructedError node record used all =
    Rule.error
        { message = "Non-exhaustive Record Destructing detected"
        , details =
            [ "You only used " ++ String.fromInt used ++ " of " ++ String.fromInt all ++ " fields from the record: " ++ record
            ]
        }
        (Node.range node)


accessError : Range -> String -> Int -> Int -> Error {}
accessError range record used all =
    Rule.error
        { message = "Non-exhaustive Record Access detected"
        , details =
            [ "You only used " ++ String.fromInt used ++ " of " ++ String.fromInt all ++ " fields from the record: " ++ record
            ]
        }
        range
