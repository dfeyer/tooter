module View.Account exposing (view)

import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Icon exposing (icon)
import Image exposing (accountImage, smallAccountImage)
import Theme exposing (Theme)
import Type exposing (..)
import View.Formatter exposing (formatContent)


view : Theme -> Account -> List (Html msg)
view theme account =
    [ div [ css [ marginBottom (rem 1) ] ] [ accountImage (Just account.avatar) ]
    , div
        [ css [ marginBottom (rem 1) ] ]
        [ div
            [ css
                [ Theme.headline theme
                , fontSize (rem 1.34)
                ]
            ]
            [ text account.display_name ]
        , div
            [ css
                [ fontWeight (int 500)
                , fontSize (rem 0.95)
                , fontStyle italic
                ]
            ]
            [ text ("@" ++ account.username) ]
        ]
    , div
        [ css [ width (pct 85) ] ]
        [ text account.note ]
    ]
