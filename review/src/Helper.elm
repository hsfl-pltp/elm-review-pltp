module Helper exposing (functionName, isBoolExpression, maybeBoolLiteralOfExpression, toModuleName)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)


functionName : Node a -> ModuleNameLookupTable -> String -> String
functionName node lookupTable func =
    case ModuleNameLookupTable.moduleNameFor lookupTable node of
        Nothing ->
            func

        Just name ->
            toModuleName name ++ "." ++ func


toModuleName : ModuleName -> String
toModuleName =
    String.join "."


isBoolExpression : Node Expression -> ModuleNameLookupTable -> Bool
isBoolExpression node lookupTable =
    case Node.value node of
        Expression.FunctionOrValue _ value ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Basics" ] ->
                    value == "True" || value == "False"

                _ ->
                    False

        _ ->
            False


maybeBoolLiteralOfExpression : Node Expression -> ModuleNameLookupTable -> Maybe Bool
maybeBoolLiteralOfExpression node lookupTable =
    case Node.value node of
        Expression.FunctionOrValue _ value ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Basics" ] ->
                    Just (value == "True")

                _ ->
                    Nothing

        _ ->
            Nothing
