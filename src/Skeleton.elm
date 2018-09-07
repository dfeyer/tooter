module Skeleton exposing
    ( Details
    , Segment
    , Warning(..)
    , view
    )

import Browser
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, id)
import Html.Styled.Lazy exposing (lazy)
import Theme exposing (Palette, Theme)
import Toolbar exposing (iconLink, inversedIconLink, majorIconLink, navigationLink)



-- NODE


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
            , zIndex (int 10)
            ]
        ]
        [ div
            [ css
                [ flex3 (int 0) (int 0) theme.layout.sidebarWidth
                , backgroundColor theme.colors.dark
                , theme.styles.headlineFontFamily
                , theme.styles.headlineFontWeight
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
