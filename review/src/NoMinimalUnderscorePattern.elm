module NoMinimalUnderscorePattern exposing (rule)

{-| Forbids the Use of the underscore pattern, if less patterns covered by the underscore pattern then a defined threshold

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Type exposing (ValueConstructor)
import List.Extra
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Reports the use of the underscore pattern, with less then 5 patterns covered.

    config =
        [ NoMinimalUnderscorePattern.rule 2
        ]


## Fail

    type MyCustomType
        = One
        | Two
        | Three

    case myCustomType of
        One ->
            ...
        _ ->
            ...


## Success

    case myCustomType of
        One ->
            ...
        Two  ->
            ...
        Three ->
            ...

-}
rule : Int -> Rule
rule threshold =
    Rule.newProjectRuleSchema "NoMinimalUnderscorePattern" (initProjectContext threshold)
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema



-- Module Visitorz


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor



--  Declaration List Visitor


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationListVisitor declarations context =
    ( [], { context | customTypes = Dict.insert [] (customTypesFromDeclarations declarations) context.customTypes } )


customTypesFromDeclarations : List (Node Declaration) -> Dict String (Set String)
customTypesFromDeclarations declarations =
    declarations
        |> List.filterMap customType
        |> Dict.fromList


customType : Node Declaration -> Maybe ( String, Set String )
customType node =
    case Node.value node of
        Declaration.CustomTypeDeclaration { name, constructors } ->
            Just ( Node.value name, typeConstructors constructors )

        _ ->
            Nothing


typeConstructors : List (Node ValueConstructor) -> Set String
typeConstructors list =
    list
        |> List.map valueConstructorName
        |> Set.fromList


valueConstructorName : Node ValueConstructor -> String
valueConstructorName (Node _ { name }) =
    Node.value name



-- expressionEnterVisitor


expressionEnterVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionEnterVisitor node context =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            ( errorsForCases node (List.map Tuple.first cases) context, context )

        _ ->
            ( [], context )


errorsForCases : Node Expression -> List (Node Pattern) -> ModuleContext -> List (Error {})
errorsForCases node patterns context =
    if allValidPattern context.lookupTable patterns then
        errorsForPattern node patterns context

    else
        []


allValidPattern : ModuleNameLookupTable -> List (Node Pattern) -> Bool
allValidPattern lookupTable patterns =
    List.all (validPattern lookupTable) patterns


validPattern : ModuleNameLookupTable -> Node Pattern -> Bool
validPattern lookupTable node =
    case Node.value node of
        Pattern.AllPattern ->
            True

        Pattern.NamedPattern _ _ ->
            validNamedPattern (ModuleNameLookupTable.moduleNameFor lookupTable node)

        _ ->
            False


validNamedPattern : Maybe ModuleName -> Bool
validNamedPattern maybe =
    case maybe of
        Just [ "Maybe" ] ->
            False

        Just [ "Result" ] ->
            False

        _ ->
            True


errorsForPattern : Node Expression -> List (Node Pattern) -> ModuleContext -> List (Error {})
errorsForPattern node patterns context =
    let
        used =
            usedConstructors patterns

        all =
            allConstructors used context.customTypes
    in
    if (List.length all - List.length used) <= context.threshold then
        [ ruleError context.threshold node used all ]

    else
        []


allConstructors : List ( ModuleName, String ) -> CustomTypes -> List String
allConstructors used types =
    case used of
        [] ->
            []

        ( moduleName, name ) :: _ ->
            allConstructorsByModuleName moduleName name types


allConstructorsByModuleName : ModuleName -> String -> CustomTypes -> List String
allConstructorsByModuleName moduleName name types =
    case Dict.get moduleName types of
        Just moduleTypes ->
            typeByConstructors name (Dict.values moduleTypes)

        Nothing ->
            []


typeByConstructors : String -> List (Set String) -> List String
typeByConstructors name moduleTypes =
    moduleTypes
        |> List.Extra.find (Set.member name)
        |> Maybe.map Set.toList
        |> Maybe.withDefault []


usedConstructors : List (Node Pattern) -> List ( ModuleName, String )
usedConstructors patterns =
    patterns
        |> List.filterMap namedPattern
        |> Set.fromList
        |> Set.toList


namedPattern : Node Pattern -> Maybe ( ModuleName, String )
namedPattern node =
    case Node.value node of
        Pattern.NamedPattern { moduleName, name } _ ->
            Just ( moduleName, name )

        _ ->
            Nothing



-- Rule Error


ruleError : Int -> Node a -> List ( ModuleName, String ) -> List String -> Error {}
ruleError threshold node used all =
    Rule.error
        { message = "To less covered cases by the underscore pattern, at lest " ++ String.fromInt threshold ++ " cases should be covered!"
        , details =
            [ "The underscore pattern should be used with care."
            , "You only covered "
                ++ String.fromInt (List.length used)
                ++ " of "
                ++ String.fromInt (List.length all)
                ++ " cases!"
            , "When you extend your custom type, then the new constructor will not be covered in the case expression."
            , "I suggest, when your custom type has less then " ++ String.fromInt threshold ++ " constructors, you should not use the underscore pattern"
            ]
        }
        (Node.range node)



-- Context


type alias CustomTypes =
    Dict ModuleName (Dict String (Set String))


type alias ProjectContext =
    { customTypes : CustomTypes
    , threshold : Int
    }


type alias ModuleContext =
    { customTypes : CustomTypes
    , threshold : Int
    , lookupTable : ModuleNameLookupTable
    }


initProjectContext : Int -> ProjectContext
initProjectContext threshold =
    { customTypes = Dict.empty
    , threshold = threshold
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContext ->
            { customTypes = projectContext.customTypes
            , threshold = projectContext.threshold
            , lookupTable = lookupTable
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleContext ->
            { customTypes = projectCustomTypes moduleContext.customTypes (Rule.moduleNameFromMetadata metadata)
            , threshold = moduleContext.threshold
            }
        )
        |> Rule.withMetadata


projectCustomTypes : CustomTypes -> ModuleName -> CustomTypes
projectCustomTypes customTypes moduleName =
    customTypes
        |> Dict.get []
        |> Maybe.withDefault Dict.empty
        |> Dict.singleton moduleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext prevContext =
    { customTypes = Dict.union newContext.customTypes prevContext.customTypes
    , threshold = newContext.threshold
    }
