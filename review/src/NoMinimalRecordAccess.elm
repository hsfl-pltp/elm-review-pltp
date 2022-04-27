module NoMinimalRecordAccess exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


type alias Context =
    { recordAliases : Dict String Int
    , usedRecords : Dict String (Dict String ( String, Set String ))
    , functionNodes : Dict String (Node Declaration)
    , currentFunction : Maybe String
    }


initialContext : Context
initialContext =
    { recordAliases = Dict.empty
    , usedRecords = Dict.empty
    , functionNodes = Dict.empty
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
                            recordParameters context typeAnnotation patterns

                        dictForRecords =
                            funcTypes
                                |> List.map (\( n, t ) -> ( n, ( t, Set.empty ) ))
                                |> Dict.fromList
                    in
                    ( destructedErrors
                    , { context
                        | usedRecords =
                            Dict.insert functionName dictForRecords context.usedRecords
                        , functionNodes =
                            Dict.insert functionName node context.functionNodes
                        , currentFunction =
                            Just functionName
                      }
                    )

                Nothing ->
                    ( [], { context | currentFunction = Nothing } )

        _ ->
            ( [], { context | currentFunction = Nothing } )


recordParameters : Context -> Node TypeAnnotation -> List (Node Pattern) -> ( List ( String, String ), List (Error {}) )
recordParameters context node list =
    let
        typesPatterns =
            typesWithPatterns node list
    in
    ( List.filterMap (undestructedRecordType context) typesPatterns
    , List.filterMap (destructedRecordPattern context) typesPatterns
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


undestructedRecordType : Context -> ( String, Node Pattern ) -> Maybe ( String, String )
undestructedRecordType context ( typeName, node ) =
    if Dict.member typeName context.recordAliases then
        case Node.value node of
            Pattern.VarPattern name ->
                Just ( name, typeName )

            _ ->
                Nothing

    else
        Nothing


destructedRecordPattern : Context -> ( String, Node Pattern ) -> Maybe (Error {})
destructedRecordPattern context ( name, node ) =
    case Node.value node of
        Pattern.RecordPattern list ->
            case Dict.get name context.recordAliases of
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
                    ( [], updateUsedRecords context functionName (Node.value recordNode) (Node.value fieldNode) )

                Expression.Application (fun :: parameters) ->
                    case Node.value fun of
                        Expression.RecordAccessFunction field ->
                            case List.head parameters of
                                Just recordNode ->
                                    ( [], updateUsedRecords context functionName (Node.value recordNode) field )

                                _ ->
                                    ( [], context )

                        _ ->
                            ( [], context )

                _ ->
                    ( [], context )

        Nothing ->
            ( [], context )


updateUsedRecords : Context -> String -> Expression -> String -> Context
updateUsedRecords context functionName record field =
    case Dict.get functionName context.usedRecords of
        Just recordsInFunction ->
            case record of
                Expression.FunctionOrValue _ recordName ->
                    let
                        updatedRecordsInFunction =
                            Dict.update recordName
                                (\set ->
                                    case set of
                                        Just ( t, oldSet ) ->
                                            Just ( t, Set.insert field oldSet )

                                        Nothing ->
                                            Nothing
                                )
                                recordsInFunction

                        updatedUsedRecords =
                            Dict.update functionName (\_ -> Just updatedRecordsInFunction) context.usedRecords
                    in
                    { context | usedRecords = updatedUsedRecords }

                _ ->
                    context

        Nothing ->
            context


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    -- Dict String (Dict String (Set String))
    -- Dict funcNm (Dict record (Set usedFld))
    Dict.foldl (functionToErrorList context) [] context.usedRecords


functionToErrorList : Context -> String -> Dict String ( String, Set String ) -> List (Error {}) -> List (Error {})
functionToErrorList context key value list =
    case Dict.get key context.functionNodes of
        Just node ->
            List.filterMap (recordToError node context.recordAliases) (Dict.toList value) ++ list

        Nothing ->
            list


recordToError : Node Declaration -> Dict String Int -> ( String, ( String, Set string ) ) -> Maybe (Error {})
recordToError node recordFieldsCount recordFieldUsed =
    let
        recordName =
            Tuple.first (Tuple.second recordFieldUsed)

        used =
            Set.size (Tuple.second (Tuple.second recordFieldUsed))
    in
    case Dict.get recordName recordFieldsCount of
        Just fieldCount ->
            if fieldCount == used then
                Nothing

            else
                Just (accessError node recordName used fieldCount)

        Nothing ->
            Nothing


destructedError : Node a -> String -> Int -> Int -> Error {}
destructedError node record used all =
    Rule.error
        { message = "Non-exhaustive Record Destructing detected"
        , details =
            [ "You only used " ++ String.fromInt used ++ " of " ++ String.fromInt all ++ " fields from the record: " ++ record
            ]
        }
        (Node.range node)


accessError : Node a -> String -> Int -> Int -> Error {}
accessError node record used all =
    Rule.error
        { message = "Non-exhaustive Record Access detected"
        , details =
            [ "You only used " ++ String.fromInt used ++ " of " ++ String.fromInt all ++ " fields from the record: " ++ record
            ]
        }
        (Node.range node)


debugMsg : Int -> Error scope
debugMsg str =
    Rule.globalError
        { message = "Debug"
        , details =
            [ String.fromInt str
            ]
        }


exprToString : Node Expression -> String
exprToString node =
    case Node.value node of
        Expression.FunctionOrValue _ str ->
            str

        _ ->
            "falscher case"
