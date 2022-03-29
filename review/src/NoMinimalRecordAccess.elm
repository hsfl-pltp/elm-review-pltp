module NoMinimalRecordAccess exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
import Review.Rule as Rule exposing (Error, Rule)


type alias Context =
    { recordAliases : Dict String Int
    , usedRecords : Dict String Int
    , isInFunctionWithRecord : Bool
    }


initialContext : Context
initialContext =
    { recordAliases = Dict.empty
    , usedRecords = Dict.empty
    , isInFunctionWithRecord = False
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
    let
        all =
            declarations
                |> List.filterMap customRecords
                |> Dict.fromList

        used =
            Dict.map (\_ _ -> 0) all
    in
    ( [], { context | recordAliases = all, usedRecords = used } )


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
                    ( []
                    , { context
                        | isInFunctionWithRecord =
                            sig
                                |> Node.value
                                |> .typeAnnotation
                                |> hasRecordInTypeAnnotation context
                      }
                    )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


hasRecordInTypeAnnotation : Context -> Node TypeAnnotation -> Bool
hasRecordInTypeAnnotation context annotation =
    case Node.value annotation of
        GenericType _ ->
            False

        Typed node nodes ->
            case nodes of
                [] ->
                    Dict.member
                        (node
                            |> Node.value
                            |> Tuple.second
                        )
                        context.recordAliases

                _ ->
                    List.foldl (||) False (List.map (hasRecordInTypeAnnotation context) nodes)

        Unit ->
            False

        Tupled nodes ->
            List.foldl (||) False (List.map (hasRecordInTypeAnnotation context) nodes)

        -- hier später nachbessern/inkludieren
        Record _ ->
            False

        -- hier später nachbessern/inkludieren
        GenericRecord _ _ ->
            False

        FunctionTypeAnnotation left right ->
            hasRecordInTypeAnnotation context left || hasRecordInTypeAnnotation context right


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    if context.isInFunctionWithRecord then
        -- Vorkommen im Context hochzählen
        ( [], context )

    else
        ( [], context )


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    -- Abgleich der gezählten Vorkommen mit der Gesamtzahl an Feldern in den records
    []
