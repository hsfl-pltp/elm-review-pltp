module UseNamingConventions exposing (rule)

{-| Forbids the use of function names starting with get

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the naming of a function starting with get

    config =
        [ UseCorrectNaming.rule
        ]


## Fail

    getName =
        ...


## Success

    name =
        ...

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "UseCorrectNaming" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            errorsForDeclaration (Node.value declaration)

        _ ->
            []


errorsForDeclaration : FunctionImplementation -> List (Error {})
errorsForDeclaration { name } =
    if String.startsWith "get" (Node.value name) then
        [ nameError name ]

    else
        []


nameError : Node String -> Error {}
nameError (Node range value) =
    Rule.error
        { message = "Function name can be improved: " ++ value
        , details =
            [ "Using the prefix `get` is a practice from object-oriented programming. In functional programming the `get` prefix is usually omitted."
            ]
        }
        range
