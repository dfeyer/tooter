module Page.SignIn exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation exposing (Key)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, placeholder, src, style, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode as Json
import Mastodon.Url
import OAuth
import OAuth.AuthorizationCode
import Skeleton
import Theme exposing (Theme)
import Url exposing (Protocol(..), Url)



-- Model


type alias Model =
    { redirectUri : Url
    , error : Maybe String
    , token : Maybe OAuth.Token
    , profile : Maybe Profile
    , state : String
    , instance : String
    }


type alias Profile =
    { name : String
    , picture : String
    }


type OAuthProvider
    = Mastodon


type alias OAuthConfiguration =
    { authorizationEndpoint : Url
    , tokenEndpoint : Url
    , profileEndpoint : Url
    , clientId : String
    , clientSecret : String
    , scope : List String
    , profileDecoder : Json.Decoder Profile
    }


makeInitModel : String -> Url -> Model
makeInitModel bytes origin =
    { redirectUri = { origin | query = Nothing, fragment = Nothing }
    , error = Nothing
    , token = Nothing
    , profile = Nothing
    , state = bytes
    , instance = "social.ttree.docker"
    }



-- INIT


init : String -> Url -> Key -> ( Model, Cmd Msg )
init randomBytes origin _ =
    let
        model =
            makeInitModel randomBytes origin
    in
    case OAuth.AuthorizationCode.parseCode origin of
        OAuth.AuthorizationCode.Empty ->
            ( model, Cmd.none )

        OAuth.AuthorizationCode.Success { code, state } ->
            if state /= Just model.state then
                ( { model | error = Just "'state' doesn't match, the request has likely been forged by an adversary!" }
                , Cmd.none
                )

            else
                ( model
                , getAccessToken model configuration model.redirectUri code
                )

        OAuth.AuthorizationCode.Error { error, errorDescription } ->
            ( { model | error = Just <| errorResponseToString { error = error, errorDescription = errorDescription } }
            , Cmd.none
            )



-- UPDATE


type Msg
    = NoOp
      -- Set the current instance name
    | SetInstance String
      -- The 'sign-in' button has been hit
    | SignInRequested OAuthConfiguration
      -- The 'sign-out' button has been hit
    | SignOutRequested
      -- Got a response from the googleapis token endpoint
    | GotAccessToken OAuthConfiguration (Result Http.Error OAuth.AuthorizationCode.AuthenticationSuccess)
      -- Got a response from the googleapis info endpoint
    | GotUserInfo (Result Http.Error Profile)


getUserInfo : Model -> OAuthConfiguration -> OAuth.Token -> Cmd Msg
getUserInfo model { profileEndpoint, profileDecoder } token =
    Http.send GotUserInfo <|
        Http.request
            { method = "GET"
            , body = Http.emptyBody
            , headers = OAuth.useToken token []
            , withCredentials = False
            , url = Url.toString { profileEndpoint | host = model.instance }
            , expect = Http.expectJson profileDecoder
            , timeout = Nothing
            }


getAccessToken : Model -> OAuthConfiguration -> Url -> String -> Cmd Msg
getAccessToken model ({ clientId, clientSecret, tokenEndpoint } as config) redirectUri code =
    Http.send (GotAccessToken config) <|
        Http.request <|
            OAuth.AuthorizationCode.makeTokenRequest
                { credentials =
                    { clientId = clientId
                    , secret = Just clientSecret
                    }
                , code = code
                , url = { tokenEndpoint | host = model.instance }
                , redirectUri = redirectUri
                }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SignInRequested { clientId, authorizationEndpoint, scope } ->
            let
                auth =
                    { clientId = clientId
                    , redirectUri = model.redirectUri
                    , scope = scope
                    , state = Just model.state
                    , url = { authorizationEndpoint | host = model.instance }
                    }
            in
            ( model
            , auth |> OAuth.AuthorizationCode.makeAuthUrl |> Url.toString |> Navigation.load
            )

        SetInstance value ->
            ( { model | instance = value }
            , Cmd.none
            )

        SignOutRequested ->
            ( model
            , Navigation.load (Url.toString model.redirectUri)
            )

        GotAccessToken config res ->
            case res of
                Err (Http.BadStatus { body }) ->
                    case Json.decodeString OAuth.AuthorizationCode.defaultAuthenticationErrorDecoder body of
                        Ok { error, errorDescription } ->
                            let
                                errMsg =
                                    "Unable to retrieve token: " ++ errorResponseToString { error = error, errorDescription = errorDescription }
                            in
                            ( { model | error = Just errMsg }
                            , Cmd.none
                            )

                        _ ->
                            ( { model | error = Just ("Unable to retrieve token: " ++ body) }
                            , Cmd.none
                            )

                Err _ ->
                    ( { model | error = Just "CORS is likely disabled on the authorization server. Unable to retrieve token: HTTP request failed." }
                    , Cmd.none
                    )

                Ok { token } ->
                    ( { model | token = Just token }
                    , getUserInfo model config token
                    )

        GotUserInfo res ->
            case res of
                Err err ->
                    ( { model | error = Just "Unable to fetch user profile ¯\\_(ツ)_/¯" }
                    , Cmd.none
                    )

                Ok profile ->
                    ( { model | profile = Just profile }
                    , Cmd.none
                    )


view : Model -> Theme -> Skeleton.Details Msg
view model theme =
    { title = "🌎 SignIn on Tooter"
    , header = []
    , warning = Skeleton.NoProblems
    , kids =
        [ viewContent theme
            { buttons =
                [ viewSignInButton model SignInRequested
                ]
            , onSignOut = SignOutRequested
            }
            model
        ]
    , css = []
    , theme = theme
    }



-- View


viewContent : Theme -> { onSignOut : msg, buttons : List (Html msg) } -> Model -> Html msg
viewContent theme { onSignOut, buttons } model =
    let
        content =
            case ( model.token, model.profile ) of
                ( Nothing, Nothing ) ->
                    viewLogin { buttons = buttons }

                ( Just token, Nothing ) ->
                    [ viewFetching ]

                ( _, Just profile ) ->
                    [ viewProfile onSignOut profile ]
    in
    viewBody theme model content


viewBody : Theme -> Model -> List (Html msg) -> Html msg
viewBody theme model content =
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent center
            , width (pct 100)
            , height (vh 80)
            ]
        ]
        (viewError theme model.error :: content)


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


viewFetching : Html msg
viewFetching =
    div
        [ style "color" "#757575"
        , style "font" "Roboto Arial"
        , style "text-align" "center"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        ]
        [ text "fetching profile..." ]


viewProfile : msg -> Profile -> Html msg
viewProfile onSignOut profile =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "justify-content" "center"
        ]
        [ img
            [ src profile.picture
            , style "height" "15em"
            , style "width" "15em"
            , style "border-radius" "50%"
            , style "box-shadow" "rgba(0,0,0,0.25) 0 0 4px 2px"
            ]
            []
        , div
            [ style "margin" "2em"
            , style "font" "24px Roboto, Arial"
            , style "color" "#757575"
            ]
            [ text <| profile.name ]
        , div
            []
            [ button
                [ onClick onSignOut
                , style "font-size" "24px"
                , style "cursor" "pointer"
                , style "height" "2em"
                , style "width" "8em"
                ]
                [ text "Sign Out"
                ]
            ]
        ]


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


viewSignInButton : Model -> (OAuthConfiguration -> Msg) -> Html Msg
viewSignInButton model onSignIn =
    div
        []
        [ div []
            [ viewInput "text" "Instance" model.instance SetInstance
            , button
                [ css
                    [ borderRadius (pct 0)
                    , fontSize (rem 1.4)
                    , border (rem 0)
                    , padding (rem 0.5)
                    ]
                , onClick (onSignIn configuration)
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


viewInput : String -> String -> String -> (String -> Msg) -> Html Msg
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



--
-- Helpers
--


errorResponseToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
errorResponseToString { error, errorDescription } =
    let
        code =
            OAuth.errorCodeToString error

        desc =
            errorDescription
                |> Maybe.withDefault ""
                |> String.replace "+" " "
    in
    code ++ ": " ++ desc


randomBytesFromState : String -> String
randomBytesFromState str =
    str
        |> stringDropLeftUntil (\c -> c == ".")


stringDropLeftUntil : (String -> Bool) -> String -> String
stringDropLeftUntil predicate str =
    let
        ( h, q ) =
            ( String.left 1 str, String.dropLeft 1 str )
    in
    if q == "" || predicate h then
        q

    else
        stringDropLeftUntil predicate q


stringLeftUntil : (String -> Bool) -> String -> String
stringLeftUntil predicate str =
    let
        ( h, q ) =
            ( String.left 1 str, String.dropLeft 1 str )
    in
    if h == "" || predicate h then
        ""

    else
        h ++ stringLeftUntil predicate q


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
    , profileEndpoint = { defaultHttpsUrl | path = Mastodon.Url.userAccount }
    , scope = [ "read", "write", "follow" ]
    , profileDecoder =
        Json.map2 Profile
            (Json.field "username" Json.string)
            (Json.field "avatar" Json.string)
    }
