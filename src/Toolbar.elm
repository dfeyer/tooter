module Toolbar exposing (iconLink, inversedIconLink, majorIconLink, navigationLink)

import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, id)
import Html.Styled.Lazy exposing (lazy)
import Icon exposing (icon, iconWithLabel)
import Theme exposing (Palette, Theme, overrideCss)


toolbarLinkStyle : Theme -> List Style
toolbarLinkStyle theme =
    [ color inherit
    , paddingLeft (rem 1)
    , transition
        [ Css.Transitions.backgroundColor 180
        , Css.Transitions.transform3 240 0 easeInOut
        ]
    , hover
        [ backgroundColor theme.colors.major
        ]
    ]


toolbarLink : Theme -> List (Html msg) -> Html msg
toolbarLink theme content =
    a
        [ css
            (toolbarLinkStyle theme)
        , href "#"
        ]
        content


inversedToolbarLink : Theme -> List (Html msg) -> Html msg
inversedToolbarLink theme content =
    a
        [ overrideCss
            (toolbarLinkStyle theme)
            [ color theme.colors.lightText
            , backgroundColor theme.colors.dark
            ]
        , href "#"
        ]
        content


majorToolbarLink : Theme -> List (Html msg) -> Html msg
majorToolbarLink theme content =
    a
        [ overrideCss
            (toolbarLinkStyle theme)
            [ color theme.colors.major
            , backgroundColor inherit
            , hover
                [ color theme.colors.lightText
                ]
            ]
        , href "#"
        ]
        content


navigationLink : Theme -> String -> String -> Html msg
navigationLink theme label iconName =
    toolbarLink theme
        [ iconWithLabel iconName label ]


iconLink : Theme -> String -> String -> Html msg
iconLink theme label iconName =
    toolbarLink theme
        [ div [ css [ marginRight (rem 1) ] ] [ icon iconName ] ]


inversedIconLink : Theme -> String -> String -> Html msg
inversedIconLink theme label iconName =
    inversedToolbarLink theme
        [ div [ css [ marginRight (rem 1) ] ] [ icon iconName ] ]


majorIconLink : Theme -> String -> String -> Html msg
majorIconLink theme label iconName =
    majorToolbarLink theme
        [ div [ css [ marginRight (rem 1) ] ] [ icon iconName ] ]
