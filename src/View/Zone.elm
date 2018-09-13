module View.Zone exposing (mainArea, sidebar, aside, full)

import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Icon exposing (icon)
import Image exposing (circularAccountImage, smallAccountImage)
import Theme exposing (Theme)
import Type exposing (..)
import View.Formatter exposing (formatContent)


mainArea : Theme -> List (Html msg) -> Html msg
mainArea { styles } content =
    div
        [ css
            [ flex (int 1)
            , styles.contentSidePadding
            , marginRight (rem 1)
            ]
        ]
        content


sidebar : Theme -> List (Html msg) -> Html msg
sidebar { layout } content =
    div
        [ css
            [ flex3 (int 0) (int 0) layout.sidebarWidth
            , paddingLeft layout.defaultMargin
            ]
        ]
        content


aside : Theme -> List (Html msg) -> Html msg
aside { layout } content =
    div
        [ css
            [ flex3 (int 0) (int 0) layout.secondSidebarWidth
            , paddingLeft layout.smallMargin
            , marginRight (rem 1)
            ]
        ]
        content

full : Theme -> List (Html msg) -> Html msg
full { layout } content =
     div
         [ css
             [ paddingLeft layout.defaultMargin
             ]
         ]
         content