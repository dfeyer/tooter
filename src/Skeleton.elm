module Skeleton exposing (Details, Navigation, Segment(..), Warning(..), minimalView, view)

import Browser
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Document exposing (Document)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, id)
import Html.Styled.Lazy exposing (lazy, lazy2)
import Theme exposing (Palette, Theme)
import Toolbar exposing (circularIconLink, iconLink, inversedIconLink, majorIconLink, navigationLink)
import Type exposing (Aside, Sidebar)



-- NODE


type alias Navigation =
    { main : List Segment
    , secondary : List Segment
    }


type alias Details msg =
    { title : String
    , navigation : Maybe Navigation
    , warning : Warning
    , styles : List Style
    , kids : List (Html msg)
    , sidebar : Sidebar msg
    , aside : Aside msg
    , theme : Theme
    }



-- SEGMENT


type alias SegmentLabel =
    String


type alias SegmentUrl =
    String


type alias SegmentIcon =
    String


type Segment
    = Text SegmentLabel
    | Link SegmentLabel SegmentUrl
    | LinkWithIcon SegmentLabel SegmentUrl SegmentIcon
    | LinkWithIconOnly SegmentLabel SegmentUrl SegmentIcon
    | LinkWithMajorIconOnly SegmentLabel SegmentUrl SegmentIcon
    | LinkWithCircularIconOnly SegmentLabel SegmentUrl SegmentIcon


type Warning
    = NoProblems



-- VIEW


view : (a -> msg) -> Details a -> Document msg
view toMsg { title, theme, warning, navigation, styles, kids } =
    { title =
        title
    , body =
        [ div
            [ id "app"
            , css
                [ maxWidth (rem 75)
                , margin2 (rem 0) (rem 4)
                , backgroundColor theme.colors.appBackground
                ]
            ]
            [ lazy viewWarning warning
            , lazy2 viewHeader theme navigation
            , Html.Styled.map toMsg <|
                div (id "main" :: [ css styles ]) kids
            , lazy viewFooter theme
            ]
        ]
    }


minimalView : Details msg -> Document msg
minimalView { title, theme, warning, navigation, styles, kids } =
    { title =
        title
    , body =
        [ div
            [ id "app"
            , css
                [ width (rem 65)
                , maxWidth (pct 75)
                ]
            ]
            [ lazy viewWarning warning
            , viewMinimalHeader theme
            , div (id "main" :: [ css styles ]) kids
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


headerWrapper : Theme -> { logo : List (Html msg), content : List (Html msg) } -> Html msg
headerWrapper theme { logo, content } =
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
                , fontSize (rem 1.4)
                ]
            ]
            content
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


viewHeader : Theme -> Maybe Navigation -> Html msg
viewHeader theme nav =
    let
        ni =
            inversedIconLink theme
    in
    headerWrapper
        theme
        { logo =
            [ viewLogo theme
            , div [ css [ marginLeft auto ] ] [ ni "/panel" "Menu" "menu" ]
            ]
        , content =
            case nav of
                Just { main, secondary } ->
                    [ viewNavigation theme main
                    , viewNavigation theme secondary
                    ]

                Nothing ->
                    []
        }


viewMinimalHeader : Theme -> Html msg
viewMinimalHeader theme =
    headerWrapper
        theme
        { logo =
            [ viewLogo theme
            ]
        , content = [ div [ css [ marginLeft (rem 1), fontSize (rem 1.35) ] ] [ text "A web client for your favorits Mastodon instances" ] ]
        }


viewNavigationItems : Theme -> List Segment -> List (Html msg)
viewNavigationItems theme nav =
    nav
        |> List.map
            (\item ->
                case item of
                    LinkWithIcon label url iconName ->
                        navigationLink theme url label iconName

                    LinkWithIconOnly label url iconName ->
                        iconLink theme url label iconName

                    LinkWithMajorIconOnly label url iconName ->
                        majorIconLink theme url label iconName

                    LinkWithCircularIconOnly label url iconName ->
                        circularIconLink theme url label iconName

                    _ ->
                        text ""
            )


viewNavigation : Theme -> List Segment -> Html msg
viewNavigation theme nav =
    div
        [ css
            [ displayFlex ]
        ]
        (viewNavigationItems theme nav)


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
        , a [ class "grey-link", href "https://github.com/dfeyer/tooter" ] [ text "Check it out" ]
        , text " © 2018 ttree agency - we make internet better for you"
        ]
