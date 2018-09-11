module Page.Problem exposing (notFound, offline, styles)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Theme exposing (Theme, headline)



-- NOT FOUND


notFound : Theme -> List (Html msg)
notFound theme =
    [ div [ css [ headline theme, fontSize (rem 4) ] ] [ text "404" ]
    , div [ css [ fontSize (rem 2) ] ] [ text "I cannot find this page!" ]
    ]


styles : List (Attribute msg)
styles =
    [ style "text-align" "center"
    , style "color" "#9A9A9A"
    , style "padding" "6em 0"
    ]



-- OFFLINE


offline : String -> List (Html msg)
offline file =
    [ div [ style "font-size" "3em" ]
        [ text "Cannot find "
        , code [] [ text file ]
        ]
    , p [] [ text "Are you offline or something?" ]
    ]
