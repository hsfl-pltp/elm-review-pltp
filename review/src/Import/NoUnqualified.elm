module Import.NoUnqualified exposing
    ( rule
    , importVisitor
    )

{-| Forbids the use of unqualified imports, expect of a white list.

@docs rule

-}

import Elm.Syntax.Exposing as Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of unqualified imports, with a whitelist of modules, which are allowed to import unqualified, like Html or Svg.

    config =
        [ OnlyQualifiedImports.rule [ "Html", "Html.Attributes" ]
        ]


## Fail

    import Foo exposing (bar)

    a =
        bar 17


## Success

    import Foo

    a =
        Foo.bar 17

-}
rule : List String -> Rule
rule whiteList =
    Rule.newModuleRuleSchema "NoUnqualifiedImports" ()
        |> Rule.withSimpleImportVisitor (importVisitor whiteList)
        |> Rule.fromModuleRuleSchema


importVisitor : List String -> Node Import -> List (Error {})
importVisitor whiteList (Node _ { moduleName, exposingList }) =
    if isWhiteListModule moduleName whiteList then
        []

    else
        errorsForExposing exposingList


isWhiteListModule : Node ModuleName -> List String -> Bool
isWhiteListModule moduleName =
    List.member (toModuleName (Node.value moduleName))


toModuleName : ModuleName -> String
toModuleName =
    String.join "."


errorsForExposing : Maybe (Node Exposing) -> List (Error {})
errorsForExposing maybeExposing =
    case maybeExposing of
        Just (Node _ (Exposing.Explicit list)) ->
            List.concatMap errorsForTopLevelExpose list

        _ ->
            []


errorsForTopLevelExpose : Node TopLevelExpose -> List (Error {})
errorsForTopLevelExpose node =
    case Node.value node of
        FunctionExpose func ->
            [ exposeError node func ]

        _ ->
            []


exposeError : Node TopLevelExpose -> String -> Error {}
exposeError node func =
    Rule.error
        { message = "This is not an qualified import: " ++ func
        , details =
            [ "A qualified import is a import, only exposing Types, like  \"import Foo exposing (MyCustomType)\""
            , "This make it easier to determine from which module the function is coming from."
            ]
        }
        (Node.range node)
