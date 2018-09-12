module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Navigation as Nav exposing (Key, pushUrl)
import Css exposing (maxWidth, pct, rem, width)
import Document exposing (Document, toUnstyledDocument)
import Html
import Html.Styled exposing (Html, button, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy exposing (lazy)
import Http
import Json.Decode as Json
import Mastodon.Url
import OAuth
import OAuth.AuthorizationCode
import Page.Home as Home
import Page.Problem as Problem
import Page.SignIn as SignIn
import Page.SignIn.Error
import Skeleton exposing (Details)
import Theme exposing (Palette, Theme, createTheme)
import Type exposing (Auth, OAuthConfiguration, Profile, initAuth)
import Url exposing (Protocol(..), Url)
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)


type alias Flags =
    { randomBytes : String }


type alias Model =
    { key : Key
    , page : Page
    , theme : Theme
    , auth : Auth
    }


type Page
    = NotFound
    | SignIn
    | Fetching
    | Home Home.Model


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view ({ auth, theme } as model) =
    case model.page of
        NotFound ->
            toUnstyledDocument <|
                Skeleton.minimalView
                    { title = "Not Found"
                    , header = []
                    , warning = Skeleton.NoProblems
                    , kids = Problem.notFound theme
                    , css = []
                    , theme = theme
                    }

        SignIn ->
            toUnstyledDocument <|
                Skeleton.minimalView
                    { title = "🌎 SignIn on Tooter"
                    , header = []
                    , warning = Skeleton.NoProblems
                    , kids =
                        [ signInView theme auth
                        ]
                    , css = []
                    , theme = theme
                    }

        Fetching ->
            toUnstyledDocument <|
                Skeleton.minimalView
                    { title = "🌎 Fetching profile"
                    , header = []
                    , warning = Skeleton.NoProblems
                    , kids =
                        [ SignIn.viewFetching theme ]
                    , css = []
                    , theme = theme
                    }

        Home home ->
            toUnstyledDocument <|
                Skeleton.view HomeMsg (Home.view home theme)


signInView : Theme -> Auth -> Html Msg
signInView theme auth =
    SignIn.view theme
        { buttons =
            [ SignIn.viewSignInButton auth SetInstance (onClick (SignInRequested SignIn.configuration))
            ]
        , onSignOut = SignOutRequested
        }
        auth



-- INIT


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { randomBytes } url key =
    stepUrl url
        { key = key
        , page = SignIn
        , theme = createTheme
        , auth = initAuth randomBytes url
        }



-- UPDATE


type
    Msg
    -- Trigger when the user click on link, default navigation
    = LinkClicked Browser.UrlRequest
      -- Trigger after an URL chang, external navigation (back/forward)
    | UrlChanged Url.Url
      -- The "home timeline" button has been hit
    | HomeMsg Home.Msg
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


updateAuthInstance : Model -> String -> Model
updateAuthInstance model value =
    let
        c =
            model.auth

        u =
            { c | instance = value }
    in
    { model | auth = u }


updateAuthError : Model -> Maybe String -> Model
updateAuthError model value =
    let
        c =
            model.auth

        u =
            { c | error = value }
    in
    { model | auth = u }


updateAuthToken : Model -> Maybe OAuth.Token -> Model
updateAuthToken model value =
    let
        c =
            model.auth

        u =
            { c | token = value }
    in
    { model | auth = u }


updateAuthProfile : Model -> Maybe Profile -> Model
updateAuthProfile model value =
    let
        c =
            model.auth

        u =
            { c | profile = value }
    in
    { model | auth = u }


update : Msg -> Model -> ( Model, Cmd Msg )
update message ({ auth } as model) =
    case message of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            stepUrl url model

        HomeMsg msg ->
            case model.page of
                Home home ->
                    stepHome model (Home.update msg home)

                _ ->
                    ( model, Cmd.none )

        SignInRequested { clientId, authorizationEndpoint, scope } ->
            ( model
            , { clientId = clientId
              , redirectUri = auth.redirectUri
              , scope = scope
              , state = Just model.auth.state
              , url = { authorizationEndpoint | host = auth.instance }
              }
                |> OAuth.AuthorizationCode.makeAuthUrl
                |> Url.toString
                |> Nav.load
            )

        SetInstance value ->
            ( updateAuthInstance model value
            , Cmd.none
            )

        SignOutRequested ->
            ( model
            , Nav.load (Url.toString auth.redirectUri)
            )

        GotAccessToken config res ->
            case res of
                Err (Http.BadStatus { body }) ->
                    case Json.decodeString OAuth.AuthorizationCode.defaultAuthenticationErrorDecoder body of
                        Ok { error, errorDescription } ->
                            let
                                errMsg =
                                    "Unable to retrieve token: " ++ Page.SignIn.Error.toString { error = error, errorDescription = errorDescription }
                            in
                            ( updateAuthError model (Just errMsg)
                            , Cmd.none
                            )

                        _ ->
                            ( updateAuthError model (Just ("Unable to retrieve token: " ++ body))
                            , Cmd.none
                            )

                Err _ ->
                    ( updateAuthError model (Just "CORS is likely disabled on the authorization server. Unable to retrieve token: HTTP request failed.")
                    , Cmd.none
                    )

                Ok { token } ->
                    ( updateAuthToken model (Just token)
                    , getUserInfo auth config token
                    )

        GotUserInfo res ->
            case res of
                Err err ->
                    ( updateAuthError model (Just "Unable to fetch user profile ¯\\_(ツ)_/¯")
                    , Cmd.none
                    )

                Ok profile ->
                    ( updateAuthProfile model (Just profile)
                    , pushUrl model.key "/"
                    )


getUserInfo : Auth -> OAuthConfiguration -> OAuth.Token -> Cmd Msg
getUserInfo auth { profileEndpoint, profileDecoder } token =
    Http.send GotUserInfo <|
        Http.request
            { method = "GET"
            , body = Http.emptyBody
            , headers = OAuth.useToken token []
            , withCredentials = False
            , url = Url.toString { profileEndpoint | host = auth.instance }
            , expect = Http.expectJson profileDecoder
            , timeout = Nothing
            }


getAccessToken : Auth -> OAuthConfiguration -> Url -> String -> Cmd Msg
getAccessToken auth ({ clientId, clientSecret, tokenEndpoint } as config) redirectUri code =
    Http.send (GotAccessToken config) <|
        Http.request <|
            OAuth.AuthorizationCode.makeTokenRequest
                { credentials =
                    { clientId = clientId
                    , secret = Just clientSecret
                    }
                , code = code
                , url = { tokenEndpoint | host = auth.instance }
                , redirectUri = redirectUri
                }



-- ROUTER


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


protectedUrl : Url.Url -> Model -> ( Model, Cmd Msg )
protectedUrl url model =
    let
        parser =
            oneOf
                [ route top
                    (stepHome model Home.init)
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url ({ auth } as model) =
    case ( auth.token, auth.profile ) of
        ( Just token, Just profile ) ->
            protectedUrl url model

        _ ->
            stepSignIn model url


stepSignIn : Model -> Url.Url -> ( Model, Cmd Msg )
stepSignIn ({ auth } as model) url =
    case OAuth.AuthorizationCode.parseCode url of
        OAuth.AuthorizationCode.Empty ->
            ( model, Cmd.none )

        OAuth.AuthorizationCode.Success { code, state } ->
            if state /= Just auth.state then
                ( updateAuthError model (Just "'state' doesn't match, the request has likely been forged by an adversary!")
                , Cmd.none
                )

            else
                ( { model | page = Fetching }
                , getAccessToken auth SignIn.configuration auth.redirectUri code
                )

        OAuth.AuthorizationCode.Error { error, errorDescription } ->
            ( updateAuthError model (Just <| Page.SignIn.Error.toString { error = error, errorDescription = errorDescription })
            , Cmd.none
            )


stepFetching : Model -> ( Model, Cmd Msg )
stepFetching model =
    ( { model | page = Fetching }
    , Cmd.none
    )


stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
stepHome model ( home, cmds ) =
    ( { model | page = Home home }
    , Cmd.map HomeMsg cmds
    )
