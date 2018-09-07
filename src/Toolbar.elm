module Toolbar exposing (circularIconLink, iconLink, inversedIconLink, majorIconLink, navigationLink)

import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, id)
import Html.Styled.Lazy exposing (lazy)
import Icon exposing (icon, iconWithLabel)
import Theme exposing (Palette, Theme, overrideCss)


linkWrapper : List (Html msg) -> List (Html msg)
linkWrapper content =
    [ div [ css [ margin2 (rem 0) (rem 1) ] ] content ]


toolbarLinkStyle : Theme -> List Style
toolbarLinkStyle theme =
    [ color inherit
    , transition
        [ Css.Transitions.backgroundColor 180
        , Css.Transitions.transform3 240 0 easeInOut
        ]
    , hover
        [ backgroundColor theme.colors.major
        , transforms [ scale 1.1 ]
        ]
    ]


toolbarLink : Theme -> List (Html msg) -> Html msg
toolbarLink theme content =
    a
        [ css
            (toolbarLinkStyle theme)
        , href "#"
        ]
        (linkWrapper content)


inversedToolbarLink : Theme -> List (Html msg) -> Html msg
inversedToolbarLink theme content =
    a
        [ overrideCss
            (toolbarLinkStyle theme)
            [ color theme.colors.lightText
            , backgroundColor theme.colors.dark
            , hover
                [ backgroundColor theme.colors.lightText
                ]
            ]
        , href "#"
        ]
        (linkWrapper content)


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
        (linkWrapper content)


circularToolbarLink : Theme -> List (Html msg) -> Html msg
circularToolbarLink theme content =
    a
        [ overrideCss
            (toolbarLinkStyle theme)
            [ color theme.colors.lightText
            , backgroundColor theme.colors.major
            , borderRadius (pct 50)
            , width theme.layout.navigationHeight
            , height theme.layout.navigationHeight
            , textAlign center
            , transforms [ scale 0.7 ]
            , hover
                [ backgroundColor theme.colors.dark
                , transforms [ scale 0.9 ]
                ]
            ]
        , href "#"
        ]
        (linkWrapper content)


navigationLink : Theme -> String -> String -> Html msg
navigationLink theme label iconName =
    toolbarLink theme
        [ iconWithLabel iconName label ]


iconLink : Theme -> String -> String -> Html msg
iconLink theme label iconName =
    toolbarLink theme
        [ icon iconName ]


inversedIconLink : Theme -> String -> String -> Html msg
inversedIconLink theme label iconName =
    inversedToolbarLink theme
        [ icon iconName ]


majorIconLink : Theme -> String -> String -> Html msg
majorIconLink theme label iconName =
    majorToolbarLink theme
        [ icon iconName ]


circularIconLink : Theme -> String -> String -> Html msg
circularIconLink theme label iconName =
    circularToolbarLink theme
        [ icon iconName ]
