module View.Formatter exposing (formatContent, textContent)

import Dict
import Html.Parser as HtmlParser
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import HtmlParserUtil
import Http
import String.Extra exposing (replace, rightOf)
import Type exposing (..)
import Url exposing (Url)
import View.Events exposing (..)


formatContent : String -> List Mention -> List (Html msg)
formatContent content mentions =
    let
        c =
            content
                |> replace " ?" "&#160;?"
                |> replace " !" "&#160;!"
                |> replace " :" "&#160;:"
                |> HtmlParser.run
    in
    case c of
        Ok c_ ->
            toVirtualDom mentions c_

        Err _ ->
            []


textContent : String -> String
textContent html =
    case HtmlParser.run html of
        Ok content ->
            HtmlParserUtil.textContent content

        Err _ ->
            ""


{-| Converts nodes to virtual dom nodes.
-}
toVirtualDom : List Mention -> List HtmlParser.Node -> List (Html msg)
toVirtualDom mentions nodes =
    List.map (toVirtualDomEach mentions) nodes


getHrefLink : List ( String, String ) -> Maybe String
getHrefLink attrs =
    attrs
        |> List.filter (\( name, _ ) -> name == "href")
        |> List.map (\( _, value ) -> value)
        |> List.head


getHashtagForLink : List ( String, String ) -> Maybe String
getHashtagForLink attrs =
    let
        hashtag =
            attrs
                |> Dict.fromList
                |> Dict.get "href"
                |> Maybe.withDefault ""
                |> rightOf "/tags/"
    in
    if hashtag /= "" then
        Just hashtag

    else
        Nothing


getMentionForLink : List ( String, String ) -> List Mention -> Maybe Mention
getMentionForLink attrs mentions =
    case getHrefLink attrs of
        Just href ->
            mentions
                |> List.filter (\m -> m.url == href)
                |> List.head

        Nothing ->
            Nothing


toVirtualDomEach : List Mention -> HtmlParser.Node -> Html msg
toVirtualDomEach mentions node =
    case node of
        -- HtmlParser.Element "a" attrs children ->
        -- createLinkNode attrs children mentions
        HtmlParser.Element name attrs children ->
            Html.Styled.node name (List.map toAttribute attrs) (toVirtualDom mentions children)

        HtmlParser.Text text_ ->
            text text_

        HtmlParser.Comment _ ->
            text ""


toAttribute : ( String, String ) -> Attribute msg
toAttribute ( name, value ) =
    attribute name value
