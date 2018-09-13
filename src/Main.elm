module Main exposing (Msg(..), main, update, view)

import Account
import Base64
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
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Mastodon.Url
import OAuth exposing (Token)
import OAuth.AuthorizationCode
import Page.Create as CreatePage
import Page.Home as HomePage exposing (TimelineMode(..))
import Page.Inbox as InboxPage
import Page.Lock as LockPage
import Page.Notification as NotificationPage
import Page.Problem as ProblemPage
import Page.Search as SearchPage
import Page.SignIn as SignInPage
import Page.SignIn.Error as SignInPageError
import Ports
import Skeleton exposing (Details)
import Theme exposing (Palette, Theme, createTheme)
import Token
import Type exposing (Account, Auth, Client, OAuthConfiguration, initAuth, resumeAuth)
import Url exposing (Protocol(..), Url)
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)


type alias Flags =
    { randomBytes : String
    , clients : String
    }


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
    | Create CreatePage.Model
    | Home HomePage.Model
    | Inbox InboxPage.Model
    | Lock LockPage.Model
    | Notification NotificationPage.Model
    | Search SearchPage.Model


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
                    , kids = ProblemPage.notFound theme
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
                    { title = "🌎 Fetching account..."
                    , header = []
                    , warning = Skeleton.NoProblems
                    , kids =
                        [ SignInPage.viewFetching theme ]
                    , css = []
                    , theme = theme
                    }

        Create create ->
            toUnstyledDocument <|
                Skeleton.view CreateMsg (CreatePage.view create theme)

        Home home ->
            toUnstyledDocument <|
                Skeleton.view HomeMsg (HomePage.view home theme)

        Inbox inbox ->
            toUnstyledDocument <|
                Skeleton.view InboxMsg (InboxPage.view inbox theme)

        Lock lock ->
            toUnstyledDocument <|
                Skeleton.view LockMsg (LockPage.view lock theme)

        Notification notification ->
            toUnstyledDocument <|
                Skeleton.view NotificationMsg (NotificationPage.view notification theme)

        Search search ->
            toUnstyledDocument <|
                Skeleton.view SearchMsg (SearchPage.view search theme)


signInView : Theme -> Auth -> Html Msg
signInView theme auth =
    SignInPage.view theme
        { buttons =
            [ SignInPage.viewSignInButton auth SetInstance (onClick (SignInRequested SignInPage.configuration))
            ]
        , onSignOut = SignOutRequested
        }
        auth



-- INIT


type Error
    = InvalidToken String


resumeClient : String -> Result Error (Maybe Client)
resumeClient clients =
    case Base64.decode clients of
        Ok clients_ ->
            case Decode.decodeString clientListDecoder clients_ of
                Ok list ->
                    -- This a workaround to get the first client only
                    -- we can change this later when we implement
                    -- multiple instance support
                    Ok (list |> List.head)

                Err _ ->
                    Err (InvalidToken "Unable to parse stored account and token")

        Err _ ->
            Err (InvalidToken "Unable to decode stored account and token")


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { randomBytes, clients } url key =
    let
        client =
            resumeClient clients

        auth =
            case resumeClient clients of
                Ok maybeClient ->
                    case maybeClient of
                        Just c ->
                            resumeAuth c randomBytes url

                        Nothing ->
                            initAuth randomBytes url

                Err _ ->
                    initAuth randomBytes url
    in
    stepUrl url
        { key = key
        , page = SignIn
        , theme = createTheme
        , auth = auth
        }



-- UPDATE


type
    Msg
    -- Trigger when the user click on link, default navigation
    = LinkClicked Browser.UrlRequest
      -- Trigger after an URL chang, external navigation (back/forward)
    | UrlChanged Url.Url
      -- The "create" button has been hit
    | CreateMsg CreatePage.Msg
      -- The "home timeline" button has been hit
    | HomeMsg HomePage.Msg
      -- The "inbox" button has been hit
    | InboxMsg InboxPage.Msg
      -- The "lock" button has been hit
    | LockMsg LockPage.Msg
      -- The "notification" button has been hit
    | NotificationMsg NotificationPage.Msg
      -- The "search" button has been hit
    | SearchMsg SearchPage.Msg
      -- Set the current instance name
    | SetInstance String
      -- The 'sign-in' button has been hit
    | SignInRequested OAuthConfiguration
      -- The 'sign-out' button has been hit
    | SignOutRequested
      -- Got a response from the googleapis token endpoint
    | GotAccessToken OAuthConfiguration (Result Http.Error OAuth.AuthorizationCode.AuthenticationSuccess)
      -- Got a response from the googleapis info endpoint
    | GotUserInfo (Result Http.Error Account)


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

        CreateMsg msg ->
            case model.page of
                Create create ->
                    stepCreate model (CreatePage.update msg create)

                _ ->
                    ( model, Cmd.none )

        HomeMsg msg ->
            case model.page of
                Home home ->
                    stepHome model (HomePage.update msg home)

                _ ->
                    ( model, Cmd.none )

        InboxMsg msg ->
            case model.page of
                Inbox inbox ->
                    stepInbox model (InboxPage.update msg inbox)

                _ ->
                    ( model, Cmd.none )

        LockMsg msg ->
            case model.page of
                Lock lock ->
                    stepLock model (LockPage.update msg lock)

                _ ->
                    ( model, Cmd.none )

        NotificationMsg msg ->
            case model.page of
                Notification notification ->
                    stepNotification model (NotificationPage.update msg notification)

                _ ->
                    ( model, Cmd.none )

        SearchMsg msg ->
            case model.page of
                Search search ->
                    stepSearch model (SearchPage.update msg search)

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
                    case Decode.decodeString OAuth.AuthorizationCode.defaultAuthenticationErrorDecoder body of
                        Ok { error, errorDescription } ->
                            let
                                errMsg =
                                    "Unable to retrieve token: " ++ SignInPageError.toString { error = error, errorDescription = errorDescription }
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
                    ( updateAuthError model (Just "Unable to fetch user account ¯\\_(ツ)_/¯")
                    , Cmd.none
                    )

                Ok account ->
                    ( updateAuthAccount model (Just account)
                    , Cmd.batch
                        [ pushUrl model.key "/"
                        , case auth.token of
                            Just token ->
                                saveClients [ Client auth.instance token account ]

                            _ ->
                                Cmd.none
                        ]
                    )



-- UPDATE HELPERS


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


updateAuthToken : Model -> Maybe Token -> Model
updateAuthToken model value =
    let
        c =
            model.auth

        u =
            { c | token = value }
    in
    { model | auth = u }


updateAuthAccount : Model -> Maybe Account -> Model
updateAuthAccount model value =
    let
        c =
            model.auth

        u =
            { c | account = value }
    in
    { model | auth = u }



-- USER INFO


getUserInfo : Auth -> OAuthConfiguration -> Token -> Cmd Msg
getUserInfo auth { accountEndpoint, accountDecoder } token =
    Http.send GotUserInfo <|
        Http.request
            { method = "GET"
            , body = Http.emptyBody
            , headers = OAuth.useToken token []
            , withCredentials = False
            , url = Url.toString { accountEndpoint | host = auth.instance }
            , expect = Http.expectJson accountDecoder
            , timeout = Nothing
            }



-- ACCESS TOKEN


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



-- CLIENT


saveClients : List Client -> Cmd Msg
saveClients clients =
    clients
        |> List.map clientEncoder
        |> Encode.list identity
        |> toJson
        |> Base64.encode
        |> Ports.saveClients


clientEncoder : Client -> Encode.Value
clientEncoder client =
    Encode.object
        [ ( "instance", Encode.string client.instance )
        , ( "token", OAuth.tokenToString client.token |> Encode.string )
        , ( "account", Account.encoder client.account )
        ]


clientDecoder : Decoder Client
clientDecoder =
    Decode.succeed Client
        |> required "instance" Decode.string
        |> required "token" Token.decoder
        |> required "account" Account.decoder


clientListDecoder : Decoder (List Client)
clientListDecoder =
    Decode.list clientDecoder



-- ROUTER


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


protectedUrl : Url.Url -> Model -> Token -> Account -> ( Model, Cmd Msg )
protectedUrl url model token account =
    let
        goHome =
            stepHome model
                (HomePage.init model.key
                    HomeTimeline
                    { instance = model.auth.instance
                    , token = token
                    , account = account
                    }
                )

        parser =
            oneOf
                [ route top
                    goHome
                , route (s "local")
                    goHome
                , route (s "federated")
                    goHome
                , route (s "create")
                    (stepCreate
                        model
                        (CreatePage.init model.key
                            { instance = model.auth.instance
                            , token = token
                            , account = account
                            }
                        )
                    )
                , route (s "inbox")
                    (stepInbox
                        model
                        (InboxPage.init model.key
                            { instance = model.auth.instance
                            , token = token
                            , account = account
                            }
                        )
                    )
                , route (s "lock")
                    (stepLock
                        model
                        (LockPage.init model.key
                            { instance = model.auth.instance
                            , token = token
                            , account = account
                            }
                        )
                    )
                , route (s "notifications")
                    (stepNotification
                        model
                        (NotificationPage.init model.key
                            { instance = model.auth.instance
                            , token = token
                            , account = account
                            }
                        )
                    )
                , route (s "search")
                    (stepSearch
                        model
                        (SearchPage.init model.key
                            { instance = model.auth.instance
                            , token = token
                            , account = account
                            }
                        )
                    )
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
    case ( auth.token, auth.account ) of
        ( Just token, Just account ) ->
            protectedUrl url model token account

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
                , getAccessToken auth SignInPage.configuration auth.redirectUri code
                )

        OAuth.AuthorizationCode.Error { error, errorDescription } ->
            ( updateAuthError model (Just <| SignInPageError.toString { error = error, errorDescription = errorDescription })
            , Cmd.none
            )


stepFetching : Model -> ( Model, Cmd Msg )
stepFetching model =
    ( { model | page = Fetching }
    , Cmd.none
    )


stepCreate : Model -> ( CreatePage.Model, Cmd CreatePage.Msg ) -> ( Model, Cmd Msg )
stepCreate model ( create, cmds ) =
    ( { model | page = Create create }
    , Cmd.map CreateMsg cmds
    )


stepHome : Model -> ( HomePage.Model, Cmd HomePage.Msg ) -> ( Model, Cmd Msg )
stepHome model ( home, cmds ) =
    ( { model | page = Home home }
    , Cmd.map HomeMsg cmds
    )


stepInbox : Model -> ( InboxPage.Model, Cmd InboxPage.Msg ) -> ( Model, Cmd Msg )
stepInbox model ( inbox, cmds ) =
    ( { model | page = Inbox inbox }
    , Cmd.map InboxMsg cmds
    )


stepLock : Model -> ( LockPage.Model, Cmd LockPage.Msg ) -> ( Model, Cmd Msg )
stepLock model ( lock, cmds ) =
    ( { model | page = Lock lock }
    , Cmd.map LockMsg cmds
    )


stepNotification : Model -> ( NotificationPage.Model, Cmd NotificationPage.Msg ) -> ( Model, Cmd Msg )
stepNotification model ( notification, cmds ) =
    ( { model | page = Notification notification }
    , Cmd.map NotificationMsg cmds
    )


stepSearch : Model -> ( SearchPage.Model, Cmd SearchPage.Msg ) -> ( Model, Cmd Msg )
stepSearch model ( search, cmds ) =
    ( { model | page = Search search }
    , Cmd.map SearchMsg cmds
    )


toJson : Encode.Value -> String
toJson =
    Encode.encode 0
