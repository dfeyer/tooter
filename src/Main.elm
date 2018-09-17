module Main exposing (Msg(..), main, update, view)

import AppRegistration
import Base64
import Browser
import Browser.Navigation as Nav exposing (Key, pushUrl)
import Client
import Css exposing (maxWidth, pct, rem, width)
import Decoder exposing (tokenDecoder)
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
import Mastodon.Decoder exposing (accountDecoder, appRegistrationDecoder)
import Mastodon.Encoder exposing (accountEncoder, appRegistrationEncoder)
import Mastodon.Url as Api
import Mastodon.OAuth exposing (initOAuthConfiguration)
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
import Request.Timeline exposing (instanceUrl)
import Skeleton exposing (Details, Navigation, Segment(..))
import Theme exposing (Palette, Theme, createTheme)
import Type exposing (Account, AppRegistration, Auth, Client, Instance, OAuthConfiguration, initAuth, resumeAuth)
import Url exposing (Protocol(..), Url)
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)


type alias Flags =
    { randomBytes : String
    , clients : String
    , registration : String
    }


type alias Model =
    { key : Key
    , currentInstance : Maybe Instance
    , page : Page
    , theme : Theme
    , auth : Auth
    , appRegistration : Maybe AppRegistration
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


defaultNavigation : Maybe Navigation
defaultNavigation =
    Just
        { main =
            [ LinkWithIcon "Home" "/" "home"
            , LinkWithIcon "Local" "/local" "paper-airplane"
            , LinkWithIcon "Federated" "/federated" "planet"
            , LinkWithIcon "Favorites" "/favorites" "star"
            , LinkWithIcon "Notifications" "/notifications" "ios-bell"
            ]
        , secondary =
            [ LinkWithIconOnly "Direct Messages" "/inbox" "email-unread"
            , LinkWithMajorIconOnly "Search" "/search" "search"
            , LinkWithCircularIconOnly "Create" "/create" "flash"
            , LinkWithIconOnly "Lock" "/lock" "android-lock"
            ]
        }


view : Model -> Browser.Document Msg
view ({ auth, theme, appRegistration } as model) =
    toUnstyledDocument <|
        case model.page of
            NotFound ->
                Skeleton.minimalView
                    { title = "Not Found"
                    , navigation = Nothing
                    , warning = Skeleton.NoProblems
                    , kids = ProblemPage.notFound theme
                    , sidebar = []
                    , aside = []
                    , styles = []
                    , theme = theme
                    }

            SignIn ->
                Skeleton.minimalView
                    { title = "🌎 SignIn on Tooter"
                    , navigation = Nothing
                    , warning = Skeleton.NoProblems
                    , kids =
                        [ signInView theme (Maybe.withDefault "" model.currentInstance) auth
                        ]
                    , sidebar = []
                    , aside = []
                    , styles = []
                    , theme = theme
                    }

            Fetching ->
                Skeleton.minimalView
                    { title = "🌎 Fetching account..."
                    , navigation = Nothing
                    , warning = Skeleton.NoProblems
                    , kids =
                        [ SignInPage.viewFetching theme ]
                    , sidebar = []
                    , aside = []
                    , styles = []
                    , theme = theme
                    }

            Create create ->
                overrideNavigation (CreatePage.view create theme) defaultNavigation
                    |> Skeleton.view CreateMsg

            Home home ->
                overrideNavigation (HomePage.view home theme) defaultNavigation
                    |> Skeleton.view HomeMsg

            Inbox inbox ->
                overrideNavigation (InboxPage.view inbox theme) defaultNavigation
                    |> Skeleton.view InboxMsg

            Lock lock ->
                overrideNavigation (LockPage.view lock theme) defaultNavigation
                    |> Skeleton.view LockMsg

            Notification notification ->
                overrideNavigation (NotificationPage.view notification theme) defaultNavigation
                    |> Skeleton.view NotificationMsg

            Search search ->
                overrideNavigation (SearchPage.view search theme) defaultNavigation
                    |> Skeleton.view SearchMsg


overrideNavigation : Details msg -> Maybe Navigation -> Details msg
overrideNavigation details nav =
    { details | navigation = nav }


signInView : Theme -> Instance -> Auth -> Html Msg
signInView theme instance auth =
    SignInPage.view theme
        { buttons =
            [ SignInPage.viewSignInButton instance SetInstance (onClick (AppRegistrationRequested auth.configuration))
            ]
        , onSignOut = SignOutRequested
        }
        auth



-- INIT


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { randomBytes, clients, registration } url key =
    let
        appRegistration =
            case AppRegistration.resume { url | path = "", query = Nothing, fragment = Nothing } registration of
                Ok a ->
                    a

                Err _ ->
                    Nothing

        oAuthConfiguration =
            initOAuthConfiguration url

        auth =
            case Client.resume clients of
                Ok maybeClient ->
                    case maybeClient of
                        Just a ->
                            resumeAuth url oAuthConfiguration a randomBytes

                        Nothing ->
                            initAuth url oAuthConfiguration randomBytes

                Err _ ->
                    initAuth url oAuthConfiguration randomBytes
    in
    stepUrl url
        { key = key
        , page = SignIn
        , currentInstance = Just "social.ttree.docker"
        , theme = createTheme
        , auth = auth
        , appRegistration = appRegistration
        }



-- UPDATE


type
    Msg
    -- Trigger when the user click on link, default navigation
    = LinkClicked Browser.UrlRequest
      -- Start OAuth2 Application registration
    | AppRegistrationRequested OAuthConfiguration
      -- OAuth2 Application registred
    | SignInRequested OAuthConfiguration (Result Http.Error AppRegistration)
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
      -- The 'sign-out' button has been hit
    | SignOutRequested
      -- Got a response from the googleapis token endpoint
    | GotAccessToken Instance OAuthConfiguration (Result Http.Error OAuth.AuthorizationCode.AuthenticationSuccess)
      -- Got a response from the googleapis info endpoint
    | GotUserInfo Instance (Result Http.Error Account)


update : Msg -> Model -> ( Model, Cmd Msg )
update message ({ auth, appRegistration } as model) =
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

        SignOutRequested ->
            ( model
            , Cmd.batch
                [ Nav.load (Url.toString auth.configuration.redirectUri)
                , AppRegistration.delete
                ]
            )

        SetInstance value ->
            ( { model | currentInstance = Just value }
            , Cmd.none
            )

        AppRegistrationRequested config ->
            case model.currentInstance of
                Just a ->
                    ( { model | page = Fetching }
                    , registerApp a config
                    )

                Nothing ->
                    ( updateAuthError model (Just "Please provide a valid instance name to connnect.")
                    , Cmd.none
                    )

        SignInRequested { authorizationEndpoint } result ->
            case result of
                Ok ({ clientId, redirectUri, scope, instance } as a) ->
                    ( { model | appRegistration = Just a }
                    , Cmd.batch
                        [ AppRegistration.save a
                        , { clientId = clientId
                          , url = { authorizationEndpoint | host = instance }
                          , redirectUri = redirectUri
                          , scope = String.split " " scope
                          , state = Just model.auth.state
                          }
                            |> OAuth.AuthorizationCode.makeAuthUrl
                            |> Url.toString
                            |> Nav.load
                        ]
                    )

                Err _ ->
                    let
                        modelWithError =
                            updateAuthError model (Just "Unable to create application, maybe your instance does not support OAuth.")
                    in
                    ( { modelWithError | appRegistration = Nothing }
                    , Cmd.none
                    )

        GotAccessToken instance config res ->
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
                    , getUserInfo instance auth config token
                    )

        GotUserInfo instance res ->
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
                                Client.save [ Client instance token account ]

                            _ ->
                                Cmd.none
                        ]
                    )



-- UPDATE HELPERS


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


getUserInfo : Instance -> Auth -> OAuthConfiguration -> Token -> Cmd Msg
getUserInfo instance auth { accountEndpoint, accountDecoder } token =
    Http.send (GotUserInfo instance) <|
        Http.request
            { method = "GET"
            , body = Http.emptyBody
            , headers = OAuth.useToken token []
            , withCredentials = False
            , url = Url.toString { accountEndpoint | host = instance }
            , expect = Http.expectJson accountDecoder
            , timeout = Nothing
            }



-- ACCESS TOKEN


registerApp : Instance -> OAuthConfiguration -> Cmd Msg
registerApp instance ({ scope, redirectUri } as config) =
    let
        scopeAsString =
            String.concat (List.intersperse " " scope)
    in
    Http.send (SignInRequested config) <|
        Http.request <|
            { method = "POST"
            , body = appRegistrationEncoder "Tooter" redirectUri scopeAsString "https://tooter.ttree.space" |> Http.jsonBody
            , headers = []
            , withCredentials = False
            , url = Url.toString (instanceUrl instance Api.apps)
            , expect = Http.expectJson (appRegistrationDecoder instance scopeAsString)
            , timeout = Nothing
            }


getAccessToken : AppRegistration -> OAuthConfiguration -> String -> Cmd Msg
getAccessToken { instance, clientId, clientSecret, redirectUri } ({ tokenEndpoint } as config) code =
    Http.send (GotAccessToken instance config) <|
        Http.request <|
            OAuth.AuthorizationCode.makeTokenRequest
                { credentials =
                    { clientId = clientId
                    , secret = Just clientSecret
                    }
                , code = code
                , url = { tokenEndpoint | host = instance }
                , redirectUri = redirectUri
                }



-- ROUTER


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


goHome : Instance -> Model -> Token -> Account -> TimelineMode -> ( Model, Cmd Msg )
goHome instance model token account msg =
    stepHome model
        (HomePage.init model.key
            msg
            { instance = instance
            , token = token
            , account = account
            }
        )


protectedUrl : Url.Url -> Model -> Token -> AppRegistration -> Account -> ( Model, Cmd Msg )
protectedUrl url model token { instance } account =
    let
        parser =
            oneOf
                [ route top
                    (goHome instance
                        model
                        token
                        account
                        HomeTimeline
                    )
                , route (s "local")
                    (goHome instance
                        model
                        token
                        account
                        LocalTimeline
                    )
                , route (s "federated")
                    (goHome instance
                        model
                        token
                        account
                        FederatedTimeline
                    )
                , route (s "favorites")
                    (goHome instance
                        model
                        token
                        account
                        FavoritesTimeline
                    )
                , route (s "create")
                    (stepCreate
                        model
                        (CreatePage.init model.key
                            { instance = instance
                            , token = token
                            , account = account
                            }
                        )
                    )
                , route (s "inbox")
                    (stepInbox
                        model
                        (InboxPage.init model.key
                            { instance = instance
                            , token = token
                            , account = account
                            }
                        )
                    )
                , route (s "lock")
                    (stepLock
                        model
                        (LockPage.init model.key
                            { instance = instance
                            , token = token
                            , account = account
                            }
                        )
                    )
                , route (s "notifications")
                    (stepNotification
                        model
                        (NotificationPage.init model.key
                            { instance = instance
                            , token = token
                            , account = account
                            }
                        )
                    )
                , route (s "search")
                    (stepSearch
                        model
                        (SearchPage.init model.key
                            { instance = instance
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
    case ( auth.token, auth.account, model.appRegistration ) of
        ( Just token, Just account, Just app ) ->
            protectedUrl url model token app account

        _ ->
            stepSignIn model url


stepSignIn : Model -> Url.Url -> ( Model, Cmd Msg )
stepSignIn ({ auth, appRegistration } as model) url =
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
                , case appRegistration of
                    Just app ->
                        getAccessToken app auth.configuration code

                    Nothing ->
                        -- todo display an error message in this case, kind of impossible state, maybe refactoring
                        Cmd.none
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
