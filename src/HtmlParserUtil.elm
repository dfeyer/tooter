module HtmlParserUtil exposing (..)

import Html.Parser exposing (Node(..))

-- Hack, need to contribute this upstream

textContent : List Node -> String
textContent nodes =
  String.join "" (List.map textContentEach nodes)


textContentEach : Node -> String
textContentEach node =
  case node of
    Element _ _ children ->
      textContent children

    Text s ->
      s

    Comment s ->
      ""