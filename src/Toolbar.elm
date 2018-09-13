module Toolbar exposing (circularIconLink, iconLink, inversedIconLink, majorIconLink, navigationLink)

import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, id)
import Html.Styled.Lazy exposing (lazy)
import Icon exposing (icon, iconWithLabel)
import Theme exposing (Palette, Theme, overrideCss)



-- TYPES


type alias LinkContent msg =
    List (Html msg)


type alias LinkLabel =
    String


type alias IconName =
    String


type alias Href =
    String



-- TOOLBAR


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


toolbarLink : Theme -> Href -> LinkContent msg -> Html msg
toolbarLink theme url content =
    a
        [ css
            (toolbarLinkStyle theme)
        , href url
        ]
        (linkWrapper content)


inversedToolbarLink : Theme -> Href -> LinkContent msg -> Html msg
inversedToolbarLink theme url content =
    a
        [ overrideCss
            (toolbarLinkStyle theme)
            [ color theme.colors.lightText
            , backgroundColor theme.colors.dark
            , hover
                [ backgroundColor theme.colors.lightText
                ]
            ]
        , href url
        ]
        (linkWrapper content)


majorToolbarLink : Theme -> Href -> LinkContent msg -> Html msg
majorToolbarLink theme url content =
    a
        [ overrideCss
            (toolbarLinkStyle theme)
            [ color theme.colors.major
            , backgroundColor inherit
            , hover
                [ color theme.colors.lightText
                ]
            ]
        , href url
        ]
        (linkWrapper content)


circularToolbarLink : Theme -> Href -> LinkContent msg -> Html msg
circularToolbarLink theme url content =
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
        , href url
        ]
        (linkWrapper content)


navigationLink : Theme -> Href -> LinkLabel -> IconName -> Html msg
navigationLink theme url label iconName =
    toolbarLink theme
        url
        [ iconWithLabel iconName label ]


iconLink : Theme -> Href -> LinkLabel -> IconName -> Html msg
iconLink theme url label iconName =
    toolbarLink theme
        url
        [ icon iconName ]


inversedIconLink : Theme -> Href -> LinkLabel -> IconName -> Html msg
inversedIconLink theme url label iconName =
    inversedToolbarLink theme
        url
        [ icon iconName ]


majorIconLink : Theme -> Href -> LinkLabel -> IconName -> Html msg
majorIconLink theme url label iconName =
    majorToolbarLink theme
        url
        [ icon iconName ]


circularIconLink : Theme -> Href -> LinkLabel -> IconName -> Html msg
circularIconLink theme url label iconName =
    circularToolbarLink theme
        url
        [ icon iconName ]



-- HELPERS


linkWrapper : List (Html msg) -> List (Html msg)
linkWrapper content =
    [ div [ css [ margin2 (rem 0) (rem 1) ] ] content ]
