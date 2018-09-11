module Skeleton exposing (Details, Segment, Warning(..), minimalView, view)

import Browser
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Document exposing (Document)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, id)
import Html.Styled.Lazy exposing (lazy)
import Theme exposing (Palette, Theme)
import Toolbar exposing (circularIconLink, iconLink, inversedIconLink, majorIconLink, navigationLink)



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


view : (a -> msg) -> Details a -> Document msg
view toMsg details =
    { title =
        details.title
    , body =
        [ div
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
        ]
    }


minimalView : Details msg -> Document msg
minimalView details =
    { title =
        details.title
    , body =
        [ div
            [ id "app"
            , css
                [ width (rem 65)
                , maxWidth (pct 75)
                ]
            ]
            [ lazy viewWarning details.warning
            , viewMinimalHeader details.header details.theme
            , div (id "main" :: [ css details.css ]) details.kids
            ]
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


headerWrapper : List Segment -> Theme -> { logo : List (Html msg), navigation : List (Html msg) } -> Html msg
headerWrapper segments theme { logo, navigation } =
    let
        ni =
            inversedIconLink theme
    in
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
                [ displayFlex
                , flex3 (int 0) (int 0) theme.layout.sidebarWidth
                , backgroundColor theme.colors.dark
                , Theme.headline theme
                , fontSize (rem 1.8)
                , color theme.colors.lightText
                ]
            ]
            logo
        , div
            [ css
                [ displayFlex
                , justifyContent spaceBetween
                , flex (int 1)
                , fontSize (rem 1.6)
                ]
            ]
            navigation
        ]


viewLogo : Theme -> Html msg
viewLogo theme =
    a
        [ href "/"
        , css
            [ display block
            , color theme.colors.lightText
            , textDecoration none
            , marginLeft theme.layout.defaultMargin
            ]
        ]
        [ text "Tooter" ]


viewHeader : List Segment -> Theme -> Html msg
viewHeader segments theme =
    let
        ni =
            inversedIconLink theme
    in
    headerWrapper segments
        theme
        { logo =
            [ viewLogo theme
            , div [ css [ marginLeft auto ] ] [ ni "Menu" "menu" ]
            ]
        , navigation =
            [ viewTimelineNavigation theme
            , viewAdvancedNavigation theme
            ]
        }


viewMinimalHeader : List Segment -> Theme -> Html msg
viewMinimalHeader segments theme =
    headerWrapper segments
        theme
        { logo =
            [ viewLogo theme
            ]
        , navigation = [ div [ css [ marginLeft (rem 1), fontSize (rem 1.35) ] ] [ text "A web client for your favorits Mastodon instances" ] ]
        }


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

        nm =
            majorIconLink theme

        nc =
            circularIconLink theme
    in
    div
        [ css
            [ displayFlex ]
        ]
        [ n "Direct Messages" "mail"
        , nm "Search" "search"
        , nc "Create" "flash"
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
