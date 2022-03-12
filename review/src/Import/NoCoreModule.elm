module Import.NoCoreModule exposing
    ( rule
    , importVisitor
    )

{-| Forbids the import of core modules, which are default imports.

@docs rule

-}

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Helper
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the import of core modules

    config =
        [ NoCoreModuleImports.rule
        ]


## Fail

    import List exposing (map)
    import String exposing (contains)


## Success

    -- No Import of List and String



-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoCoreModuleImports" ()
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.fromModuleRuleSchema


coreModules : List String
coreModules =
    [ "Basics"
    , "List"
    , "Maybe"
    , "Result"
    , "String"
    , "Char"
    , "Tuple"
    , "Debug"
    , "Platform"
    , "Platform.Cmd"
    , "Platform.Sub"
    ]


importVisitor : Node Import -> List (Error {})
importVisitor (Node _ { moduleName }) =
    errorsForModuleName moduleName


errorsForModuleName : Node ModuleName -> List (Error {})
errorsForModuleName (Node range moduleName) =
    if List.member (Helper.toModuleName moduleName) coreModules then
        [ importError range (Helper.toModuleName moduleName) ]

    else
        []


importError : Range -> String -> Error {}
importError range name =
    Rule.error
        { message = "Import of core module detected: " ++ name
        , details =
            [ "The import of a core module is not necessary because it is imported implicitly anyway. For a list of all core modules and how they imported implicitly take a look at https://package.elm-lang.org/packages/elm/core/latest/."
            ]
        }
        range
