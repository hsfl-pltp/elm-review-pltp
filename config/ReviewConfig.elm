module ReviewConfig exposing (config)

{-| This file configures elm-review

Please do not change anything here!

-}

import Import.NoCoreModule
import Import.NoUnqualified
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoForbiddenFeatures
import NoImportingEverything
import NoMinimalRecordAccess
import NoMinimalUnderscorePattern
import NoMissingTypeAnnotation
import NoNegationOfBooleanOperator
import NoPrimitiveTypeAlias
import NoRecursiveUpdate
import NoSimpleLetBody
import NoSinglePatternCase
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUselessSubscriptions
import Review.Rule exposing (Rule)
import Simplify
import UseCamelCase
import UseCommutingConversions
import UseConstantsForStyle
import UseEtaReductions exposing (ErrorStyle(..))
import UseInvertedOperators
import UseLogicalOperators
import UseNamingConventions
import UseRecordUpdate


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
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoExposingEverything.rule
    , NoForbiddenFeatures.rule
        { operators = []
        , functions =
            [ "List.map"
            , "Decode.andThen"
            ]
        , letIn = False
        , algebraicDataTypes = False
        , lambda = False
        }
    , NoImportingEverything.rule []
    , NoMinimalRecordAccess.rule
    , NoMinimalUnderscorePattern.rule 3
    , NoMissingTypeAnnotation.rule
    , NoNegationOfBooleanOperator.rule
    , NoPrimitiveTypeAlias.rule
    , NoRecursiveUpdate.rule
    , NoSimpleLetBody.rule
    , NoSinglePatternCase.rule NoSinglePatternCase.fixInArgument
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoUselessSubscriptions.rule
    , Simplify.rule Simplify.defaults
    , UseCommutingConversions.rule
    , UseConstantsForStyle.rule
    , UseCamelCase.rule UseCamelCase.default
    , UseEtaReductions.rule LocatedError
    , UseInvertedOperators.rule
    , UseNamingConventions.rule
    , UseLogicalOperators.rule
    , UseRecordUpdate.rule
    ]
