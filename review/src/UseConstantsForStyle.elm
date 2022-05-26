module UseConstantsForStyle exposing (rule)

{-| Forbids the use of ungrouped style elements

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Helper
import List.Extra
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of ungrouped style elements

    config =
        [ UseConstantsForStyle.rule
        ]


## Fail

    a =
        div [ text "hello world" ]


## Success

    aStyle : List (Attribute msg)
    aStyle =
        [ style "padding" "0", style "margin" "0" ]

    a =
        div aStyle [ text "hello world" ]

-}



-- Context


htmlElements : List String
htmlElements =
    [ "text"
    , "node"
    , "map"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h5"
    , "h6"
    , "div"
    , "p"
    , "hr"
    , "pre"
    , "blockquote"
    , "span"
    , "a"
    , "code"
    , "em"
    , "strong"
    , "i"
    , "b"
    , "u"
    , "sub"
    , "sup"
    , "br"
    , "ol"
    , "ul"
    , "li"
    , "dl"
    , "dt"
    , "dd"
    , "img"
    , "iframe"
    , "canvas"
    , "math"
    , "form"
    , "input"
    , "textarea"
    , "button"
    , "select"
    , "option"
    , "section"
    , "nav"
    , "article"
    , "aside"
    , "header"
    , "footer"
    , "address"
    , "main_"
    , "figure"
    , "figcaption"
    , "table"
    , "caption"
    , "colgroup"
    , "col"
    , "tbody"
    , "thead"
    , "tfoot"
    , "tr"
    , "td"
    , "th"
    , "fieldset"
    , "legend"
    , "label"
    , "datalist"
    , "optgroup"
    , "output"
    , "progress"
    , "meter"
    , "audio"
    , "video"
    , "source"
    , "track"
    , "embed"
    , "object"
    , "param"
    , "ins"
    , "del"
    , "small"
    , "cite"
    , "dfn"
    , "abbr"
    , "time"
    , "var"
    , "samp"
    , "kbd"
    , "s"
    , "q"
    , "mark"
    , "ruby"
    , "rt"
    , "rp"
    , "bdi"
    , "bdo"
    , "wbr"
    , "details"
    , "summary"
    , "menuitem"
    , "menu"
    ]


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "UseConstantsForStyle" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema



-- Context


type alias Context =
    { lookupTable : ModuleNameLookupTable }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable }
        )
        |> Rule.withModuleNameLookupTable



-- Expression visitor


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.Application (element :: attributes :: _) ->
            ( errorsForHtmlElement element attributes context, context )

        _ ->
            ( [], context )


errorsForHtmlElement : Node Expression -> Node Expression -> Context -> List (Error {})
errorsForHtmlElement node attributes context =
    if isHtmlElement node context.lookupTable then
        errorsForAttributes attributes context

    else
        []


isHtmlElement : Node Expression -> ModuleNameLookupTable -> Bool
isHtmlElement node lookupTable =
    case Node.value node of
        Expression.FunctionOrValue _ func ->
            isFromHtmlModule (ModuleNameLookupTable.moduleNameFor lookupTable node)
                && List.member func htmlElements

        _ ->
            False


errorsForAttributes : Node Expression -> Context -> List (Error {})
errorsForAttributes node context =
    case Node.value node of
        Expression.ListExpr nodes ->
            if List.Extra.count (isStyleAttribute context.lookupTable) nodes >= 2 then
                [ ruleError node ]

            else
                []

        _ ->
            []


isStyleAttribute : ModuleNameLookupTable -> Node Expression -> Bool
isStyleAttribute lookupTable node =
    case Node.value node of
        Expression.FunctionOrValue _ func ->
            Helper.functionName node lookupTable func == "Html.Attributes.style"

        Expression.Application (x :: _) ->
            isStyleAttribute lookupTable x

        _ ->
            False


isFromHtmlModule : Maybe ModuleName -> Bool
isFromHtmlModule name =
    case name of
        Just moduleName ->
            [ "Html" ] == moduleName

        Nothing ->
            False


ruleError : Node Expression -> Error {}
ruleError node =
    Rule.error
        { message = "Group styles into constants"
        , details =
            [ "To structure your code you should use a constant for styles. Move all style attributes into constants. For example, use `header = [ style ..., style ... ]`"
            ]
        }
        (Node.range node)
