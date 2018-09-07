module Skeleton exposing
    ( Details
    , Segment
    , Theme
    , Warning(..)
    , createTheme
    , view
    )

import Browser
import ColorPalette exposing (Palette)
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, id)
import Html.Styled.Lazy exposing (lazy)



-- NODE


type alias Theme =
    { colors : Palette
    , layout : Layout
    , styles : Styles
    }


type alias Layout =
    { defaultMargin : Rem
    , smallMargin : Rem
    , compactMargin : Rem
    , navigationHeight : Rem
    }


type alias Styles =
    { navigationHeight : Style
    , contentPadding : Style
    , blockMargin : Style
    }


createTheme : Palette -> Theme
createTheme palette =
    let
        layout =
            { defaultMargin = rem 1.25
            , smallMargin = rem 0.75
            , compactMargin = rem 0.75
            , navigationHeight = rem 3.25
            }

        styles =
            { navigationHeight = height layout.navigationHeight
            , contentPadding = padding2 layout.smallMargin layout.compactMargin
            , blockMargin = margin2 layout.smallMargin layout.compactMargin
            }
    in
    { colors = palette
    , layout = layout
    , styles = styles
    }


type alias Details msg =
    { title : String
    , header : List Segment
    , warning : Warning
    , css : List Style
    , kids : List (Html msg)
    , theme : Theme
    }



-- SEGMENT


type Segment
    = Text String
    | Link String String


type Warning
    = NoProblems



-- ICON


icon : String -> Html msg
icon name =
    i [ class ("icon ion-md-" ++ name) ] []


iconWithLabel : String -> String -> Html msg
iconWithLabel name label =
    div
        [ css
            [ paddingRight (rem 1.25)
            ]
        ]
        [ icon name
        , span
            [ css
                [ display inlineBlock, marginLeft (rem 0.5), fontSize (pct 65), verticalAlign top ]
            ]
            [ text label ]
        ]



-- VIEW


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title =
        details.title
    , body =
        [ toUnstyled
            (div
                [ id "app"
                , css
                    [ width (rem 65)
                    , maxWidth (pct 75)
                    , backgroundColor details.theme.colors.appBackground
                    ]
                ]
                [ lazy viewWarning details.warning
                , viewHeader details.header details.theme
                , Html.Styled.map toMsg <|
                    div (id "main" :: [ css details.css ]) details.kids
                , viewFooter details.theme
                ]
            )
        ]
    }



-- VIEW WARNING


viewWarning : Warning -> Html msg
viewWarning warning =
    div [ id "warnings" ] <|
        case warning of
            NoProblems ->
                []



-- VIEW HEADER


viewHeader : List Segment -> Theme -> Html msg
viewHeader segments theme =
    div
        [ id "header"
        , css
            [ displayFlex
            , position sticky
            , top (px 0)
            , backgroundColor theme.colors.mediumBackground
            , lineHeight (rem 3.6)
            , theme.styles.navigationHeight
            ]
        ]
        [ div
            [ css
                [ flex3 (int 0) (int 0) (px 240)
                , backgroundColor theme.colors.dark
                , fontFamilies [ "Work Sans", "serif" ]
                , fontWeight (int 900)
                , fontSize (rem 1.8)
                , color theme.colors.lightText
                , paddingLeft theme.layout.smallMargin
                ]
            ]
            [ text "Tooter" ]
        , div
            [ css
                [ displayFlex
                , justifyContent spaceBetween
                , flex (int 1)
                , fontSize (rem 1.6)
                ]
            ]
            [ viewTimelineNavigation theme
            , viewAdvancedNavigation theme
            ]
        ]


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


overrideCss : List Style -> List Style -> Attribute msg
overrideCss styles overrides =
    css (List.concat [ styles, overrides ])


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


viewTimelineNavigation : Theme -> Html msg
viewTimelineNavigation theme =
    let
        n =
            navigationLink theme
    in
    div
        [ css
            [ displayFlex ]
        ]
        [ n "Home" "home"
        , n "Notifications" "notifications-outline"
        , n "Local" "paper"
        , n "Federated" "planet"
        ]


viewAdvancedNavigation : Theme -> Html msg
viewAdvancedNavigation theme =
    let
        n =
            iconLink theme

        ni =
            inversedIconLink theme

        nm =
            majorIconLink theme
    in
    div
        [ css
            [ displayFlex ]
        ]
        [ n "Direct Messages" "mail"
        , n "Listes" "list-box"
        , nm "Search" "search"
        , ni "Menu" "menu"
        ]



-- VIEW FOOTER


viewFooter : Theme -> Html msg
viewFooter theme =
    div
        [ id "footer"
        , css
            [ fontSize (rem 0.9)
            , backgroundColor theme.colors.mediumBackground
            , theme.styles.contentPadding
            ]
        ]
        [ text "All code for this site is open source and written in Elm. "
        , a [ class "grey-link", href "https://github.com/ttreeagency/tooter/" ] [ text "Check it out" ]
        , text " © 2018 ttree agency - we make internet better for you"
        ]
