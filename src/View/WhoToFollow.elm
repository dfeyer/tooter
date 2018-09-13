module View.WhoToFollow exposing (view)

import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Icon exposing (icon)
import Image exposing (circularAccountImage, smallAccountImage)
import Theme exposing (Theme)
import Type exposing (..)
import View.Formatter exposing (formatContent)


view : Theme -> Account -> List (Html msg)
view theme account =
    [ div []
        [ div [ css [ Theme.headline theme, fontSize (rem 1.3) ] ] [ text "Who to follow" ]
        , div [ css [ margin2 theme.layout.defaultMargin (rem 0) ] ]
            [ asideLink theme "refresh" "Refresh"
            , asideLink theme "eye" "View all"
            ]
        , accountSuggestionList theme
        ]
    ]


accountSuggestionList : Theme -> Html msg
accountSuggestionList theme =
    div []
        [ accountSuggestion theme (Just "https://picsum.photos/200/300?random")
        , accountSuggestion theme (Just "https://picsum.photos/200/300?random")
        , accountSuggestion theme (Just "https://picsum.photos/200/300?random")
        ]


accountSuggestion : Theme -> Maybe String -> Html msg
accountSuggestion theme maybeImage =
    div [ css [ marginBottom (rem 1.5) ] ]
        [ div
            [ css
                [ displayFlex
                , cursor pointer
                , backgroundColor theme.colors.lightBackground
                , borderRadius (rem 1.25)
                , width (rem 8)
                , height (rem 2)
                , marginBottom (rem 0.25)
                , alignItems center
                , overflow hidden
                , opacity (num 0.6)
                , transition
                    [ Css.Transitions.backgroundColor 180
                    , Css.Transitions.opacity 220
                    ]
                , hover
                    [ backgroundColor theme.colors.major
                    , opacity (num 1.0)
                    ]
                ]
            ]
            [ circularAccountImage maybeImage
            , span [ css [ marginLeft (rem 0.5) ] ] [ icon "person-add" ]
            , span [ css [ marginLeft (rem 0.5) ] ] [ text "Add" ]
            ]
        , div [ css [ fontSize (pct 90), opacity (num 0.6) ] ]
            [ div [ css [ Theme.headline theme ] ] [ text "Dominique Feyer" ]
            , div [ css [ fontSize (pct 90) ] ] [ text "@dfeyer@social.ttree.ch" ]
            ]
        ]


asideLink : Theme -> String -> String -> Html msg
asideLink { colors } iconName label =
    a
        [ href "#"
        , css
            [ display block
            , fontWeight (int 500)
            , color colors.accent
            , textDecoration none
            ]
        ]
        [ icon iconName, span [ css [ marginLeft (rem 0.5) ] ] [ text label ] ]
