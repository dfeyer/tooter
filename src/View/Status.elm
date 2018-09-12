module View.Status exposing (viewStatus)

import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Icon exposing (icon)
import Image exposing (smallAccountImage)
import Theme exposing (Theme)
import Type exposing (..)
import View.Formatter exposing (formatContent)


viewStatus : Theme -> Status -> Html msg
viewStatus theme { account, content, mentions } =
    div
        [ css
            [ displayFlex
            , marginBottom (rem 2)
            ]
        ]
        [ div [ css [ marginRight (rem 1) ] ] [ smallAccountImage (Just account.avatar) ]
        , div []
            [ statusAccount theme account
            , statusContent theme content mentions
            , statusActionGroup theme
            ]
        ]


statusContent : Theme -> String -> List Mention -> Html msg
statusContent theme content mentions =
    div
        [ css
            [ marginTop (rem 0.25) ]
        ]
        (formatContent content mentions)


statusMedia : Theme -> String -> Html msg
statusMedia theme content =
    div [ css [ marginTop (rem 0.25) ] ] []


statusAccount : Theme -> Account -> Html msg
statusAccount theme { display_name, username } =
    div []
        [ span
            [ css
                [ Theme.headline theme
                ]
            ]
            [ text display_name ]
        , span [ css [ opacity (num 0.2) ] ] [ text " | " ]
        , span
            [ css
                [ fontWeight (int 500)
                , fontSize (rem 0.95)
                , fontStyle italic
                ]
            ]
            [ span [] [ text ("@" ++ username) ]

            -- , span [ css [ opacity (num 0.4) ] ] [ text "@" ]
            -- , span [ css [ opacity (num 0.4) ] ] [ text instance ]
            ]
        ]


statusActionGroup : Theme -> Html msg
statusActionGroup theme =
    div
        [ css [ displayFlex, fontSize (rem 1.2), marginTop (rem 0.35), justifyContent end ] ]
        [ statusActionGroupLink theme "Send a resonse..." "arrow-return-left"
        , statusActionGroupLink theme "Boost" "loop"
        , statusActionGroupLink theme "Add to your favorits" "android-star"
        ]


statusActionGroupLink : Theme -> String -> String -> Html msg
statusActionGroupLink theme label iconName =
    a
        [ href "#"
        , css
            [ display block
            , color theme.colors.darkText
            , marginLeft (rem 1.5)
            , opacity (num 0.5)
            , transition
                [ Css.Transitions.opacity 180
                , Css.Transitions.transform3 240 0 easeInOut
                ]
            , hover
                [ opacity (int 1)
                , transforms [ scale 1.2 ]
                ]
            ]
        ]
        [ icon iconName ]
