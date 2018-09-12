module Page.SignIn exposing (configuration, view, viewFetching, viewSignInButton)

import Browser.Navigation as Navigation exposing (Key)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, placeholder, src, style, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode as Json exposing (Decoder)
import Mastodon.Decoder exposing (accountDecoder)
import Mastodon.Url
import OAuth
import OAuth.AuthorizationCode
import Skeleton
import Theme exposing (Theme)
import Type exposing (Account, Auth, OAuthConfiguration)
import Url exposing (Protocol(..), Url)


view : Theme -> { onSignOut : msg, buttons : List (Html msg) } -> Auth -> Html msg
view theme { onSignOut, buttons } auth =
    viewBody theme auth (viewLogin { buttons = buttons })


viewBody : Theme -> Auth -> List (Html msg) -> Html msg
viewBody theme auth content =
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent center
            , width (pct 100)
            , height (vh 80)
            ]
        ]
        (viewError theme auth.error :: content)


viewLogin : { buttons : List (Html msg) } -> List (Html msg)
viewLogin { buttons } =
    [ div
        [ css
            [ displayFlex
            , alignItems flexEnd
            , justifyContent center
            , flexDirection column
            ]
        ]
        buttons
    ]


viewFetching : Theme -> Html msg
viewFetching theme =
    div
        [ css
            [ displayFlex
            , justifyContent center
            , alignItems center
            , marginTop (rem 2)
            ]
        ]
        [ div [] [ text "Fetching..." ] ]


viewError : Theme -> Maybe String -> Html msg
viewError theme error =
    case error of
        Nothing ->
            div [ style "display" "none" ] []

        Just msg ->
            div
                [ css
                    [ position fixed
                    , bottom (px 0)
                    , left (px 0)
                    , right (px 0)
                    , padding (rem 1)
                    , backgroundColor theme.colors.warning
                    , color theme.colors.lightText
                    , fontWeight (int 500)
                    ]
                ]
                [ span
                    [ css
                        [ display inlineBlock
                        , marginRight (rem 0.25)
                        , fontWeight (int 700)
                        , fontSize (pct 130)
                        ]
                    ]
                    [ text "⚠" ]
                , text msg
                ]


viewSignInButton : Auth -> (String -> msg) -> Attribute msg -> Html msg
viewSignInButton auth setInstanceMsg onSignIn =
    div
        []
        [ div []
            [ viewInput "text" "Instance" auth.instance setInstanceMsg
            , button
                [ css
                    [ borderRadius (pct 0)
                    , fontSize (rem 1.4)
                    , border (rem 0)
                    , padding (rem 0.5)
                    ]
                , onSignIn
                ]
                [ text "Sign in" ]
            ]
        , div
            [ css
                [ width (rem 22)
                , padding2 (rem 0.25) (rem 0.5)
                ]
            ]
            [ text "You'll be redirected to that server to authenticate yourself. We don't have access to your password. We respect your privacy, we don't store anything." ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input
        [ css
            [ borderRadius (pct 0)
            , fontSize (rem 1.4)
            , border (rem 0)
            , padding (rem 0.5)
            , width (rem 18)
            ]
        , type_ t
        , placeholder p
        , value v
        , onInput toMsg
        ]
        []


configuration : OAuthConfiguration
configuration =
    let
        defaultHttpsUrl =
            { protocol = Https
            , host = ""
            , path = ""
            , port_ = Nothing
            , query = Nothing
            , fragment = Nothing
            }
    in
    { clientId = "gxEufHWdQkhfvxkaRpvTRgCQFoOa03_OusYy5_uzBMw="
    , clientSecret = "N8JGmR3_-IT6y9-DQE_13t5DQlUYjJKTx2GEsQt0IXc="
    , authorizationEndpoint = { defaultHttpsUrl | path = Mastodon.Url.oauthAuthorize }
    , tokenEndpoint = { defaultHttpsUrl | path = Mastodon.Url.oauthToken }
    , accountEndpoint = { defaultHttpsUrl | path = Mastodon.Url.userAccount }
    , scope = [ "read", "write", "follow" ]
    , accountDecoder = accountDecoder
    }
