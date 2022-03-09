module ReviewConfig exposing (config)

{-| This file configures elm-review

Please do not change anything here!

-}

import Import.NoCoreModule
import Import.NoUnqualified
import NoBooleanComparison
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoForbiddenFeatures
import NoImportingEverything
import NoMinimalRecordAccess
import NoMinimalUnderscorePattern
import NoMissingTypeAnnotation
import NoNegationOfBooleanOperator
import NoSinglePatternCase
import NoUnnecessaryIf
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import UseCamelCase
import UseCommutingConversions
import UseConstantsForStyle
import UseEtaReductions
import UseInvertedOperators
import UseLogicalOperators
import UseNamingConventions


config : List Rule
config =
    [ Import.NoCoreModule.rule
    , Import.NoUnqualified.rule
        [ "Html"
        , "Html.Attributes"
        , "Html.Events"
        , "Svg"
        , "Svg.Attributes"
        ]
    , NoBooleanComparison.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoExposingEverything.rule
    , NoForbiddenFeatures.rule
        { operators = []
        , functions =
            [ "List.map"
            , "Html.Attributes.class"
            , "Maybe.withDefault"
            , "Decode.andThen"
            ]
        , letIn = False
        , algebraicDataTypes = False
        , lambda = False
        }
    , NoImportingEverything.rule []
    , NoMinimalRecordAccess.rule
        { threshold = 2
        , ignoreFunctions = [ "subscriptions" ]
        }
    , NoMinimalUnderscorePattern.rule 4
    , NoMissingTypeAnnotation.rule
    , NoNegationOfBooleanOperator.rule
    , NoSinglePatternCase.rule
    , NoUnnecessaryIf.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , UseCommutingConversions.rule
    , UseConstantsForStyle.rule
    , UseCamelCase.rule UseCamelCase.default
    , UseEtaReductions.rule False
    , UseInvertedOperators.rule
    , UseNamingConventions.rule
    , UseLogicalOperators.rule
    ]
