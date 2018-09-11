module Toot exposing (Toot, viewToot)

import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Icon exposing (icon)
import Image exposing (smallProfileImage)
import Theme exposing (Theme)


type alias Account =
    { identifier : String
    , fullname : String
    , image : Maybe String
    }


type alias Toot =
    { identifier : String
    , content : String
    , author : Account
    }


viewToot : Theme -> Toot -> Html msg
viewToot theme { author, content } =
    div
        [ css [ displayFlex, marginBottom (rem 2) ] ]
        [ div [ css [ marginRight (rem 1) ] ] [ smallProfileImage author.image ]
        , div []
            [ tootAccount theme author
            , tootContent theme content
            , tootBar theme
            ]
        ]


tootContent : Theme -> String -> Html msg
tootContent theme content =
    div [ css [ marginTop (rem 0.25) ] ] [ text content ]


tootMedia : Theme -> String -> Html msg
tootMedia theme content =
    div [ css [ marginTop (rem 0.25) ] ] []


tootAccount : Theme -> Account -> Html msg
tootAccount theme { fullname, identifier } =
    let
        splittedIdentifier =
            String.split "@" identifier |> List.filter (\y -> y /= "")

        username =
            Maybe.withDefault "" (List.head splittedIdentifier)

        instance =
            Maybe.withDefault "" (List.reverse splittedIdentifier |> List.head)
    in
    div []
        [ span
            [ css
                [ Theme.headline theme
                ]
            ]
            [ text fullname ]
        , span [ css [ opacity (num 0.2) ] ] [ text " | " ]
        , span
            [ css
                [ fontWeight (int 500)
                , fontSize (rem 0.95)
                , fontStyle italic
                ]
            ]
            [ span [] [ text username ]
            , span [ css [ opacity (num 0.4) ] ] [ text "@" ]
            , span [ css [ opacity (num 0.4) ] ] [ text instance ]
            ]
        ]


tootBar : Theme -> Html msg
tootBar theme =
    div
        [ css [ displayFlex, fontSize (rem 1.2), marginTop (rem 0.35), justifyContent end ] ]
        [ tootBarLink theme "Send a resonse..." "return-left"
        , tootBarLink theme "Boost" "refresh"
        , tootBarLink theme "Add to your favorits" "star-outline"
        ]


tootBarLink : Theme -> String -> String -> Html msg
tootBarLink theme label iconName =
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
