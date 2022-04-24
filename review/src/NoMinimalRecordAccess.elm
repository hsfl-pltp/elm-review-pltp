module NoMinimalRecordAccess exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
import Review.Rule as Rule exposing (Error, Rule)


type alias Context =
    { recordAliases : Dict String Int
    , usedRecords : Dict String (Dict String Int)
    , currentFunction : Maybe String
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
                                |> List.map (\r -> ( r, 0 ))
                                |> Dict.fromList
                    in
                    ( destructedErrors
                    , { context
                        | usedRecords =
                            Dict.union context.usedRecords (Dict.insert functionName dictForRecords Dict.empty)
                        , currentFunction =
                            Just functionName
                      }
                    )

                Nothing ->
                    ( [], { context | currentFunction = Nothing } )

        _ ->
            ( [], { context | currentFunction = Nothing } )


recordParameters : Context -> Node TypeAnnotation -> List (Node Pattern) -> ( List String, List (Error {}) )
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


undestructedRecordType : Context -> ( String, Node Pattern ) -> Maybe String
undestructedRecordType context ( name, node ) =
    if Dict.member name context.recordAliases then
        case Node.value node of
            Pattern.RecordPattern _ ->
                Nothing

            _ ->
                Just name

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
                        Just (patternError node name (List.length list) numberOfFields)

                Nothing ->
                    Nothing

        _ ->
            Nothing


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case context.currentFunction of
        Just functionName ->
            case Node.value node of
                -- Welche Expressions? -> RecordAccess, RecordAccessFunction
                -- Was ist mit destructed Records? -> wird nur in declaration abgehandelt
                _ ->
                    ( [], context )

        Nothing ->
            ( [], context )


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    -- Abgleich der gezählten Vorkommen mit der Gesamtzahl an Feldern in den records
    []


patternError : Node a -> String -> Int -> Int -> Error {}
patternError node record used all =
    Rule.error
        { message = "Non-exhaustive Record Access detected"
        , details =
            [ "You only used " ++ String.fromInt used ++ " of " ++ String.fromInt all ++ " Recordfields from " ++ record
            ]
        }
        (Node.range node)
