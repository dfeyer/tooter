module Icon exposing (icon, iconWithLabel)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)


icon : String -> Html msg
icon name =
    i [ class ("ion-" ++ name) ] []


iconWithLabel : String -> String -> Html msg
iconWithLabel name label =
    div
        []
        [ icon name
        , span
            [ css
                [ display inlineBlock, marginLeft (rem 0.5), fontSize (pct 65), verticalAlign top ]
            ]
            [ text label ]
        ]
