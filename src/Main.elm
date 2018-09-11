module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Navigation as Nav
import Html
import Html.Styled exposing (Html, button, div, text)
import Http
import Json.Decode exposing (Decoder)
import OAuth
import Page.Home as Home
import Page.Problem as Problem
import Page.SignIn as SignIn
import Skeleton
import Theme exposing (Palette, Theme, createTheme)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)


type alias Flags =
    { randomBytes : String }


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



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    , theme : Theme
    , token : Maybe OAuth.Token
    , state : String
    , error : Maybe String
    }



--- TYPES


type Page
    = NotFound
    | Home Home.Model
    | SignIn SignIn.Model


type alias Profile =
    { name : String
    , picture : String
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            Skeleton.view never
                { title = "Not Found"
                , header = []
                , warning = Skeleton.NoProblems
                , kids = Problem.notFound
                , css = []
                , theme = model.theme
                }

        Home home ->
            Skeleton.view HomeMsg (Home.view home model.theme)

        SignIn signIn ->
            Skeleton.signInView SignInMsg (SignIn.view signIn model.theme)



-- INIT


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { randomBytes } url key =
    stepUrl url
        { key = key
        , page = NotFound
        , theme = createTheme
        , token = Nothing
        , state = randomBytes
        , error = Nothing
        }



-- UPDATE


type
    Msg
    -- No operation, terminal case
    = NoOp
      -- Trigger when the user click on link, default navigation
    | LinkClicked Browser.UrlRequest
      -- Trigger after an URL chang, external navigation (back/forward)
    | UrlChanged Url.Url
      -- The "home timeline" button has been hit
    | HomeMsg Home.Msg
      -- Show the sign in page
    | SignInMsg SignIn.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

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

        SignInMsg msg ->
            case model.page of
                SignIn signIn ->
                    stepSignIn model (SignIn.update msg signIn)

                _ ->
                    ( model, Cmd.none )


stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
stepHome model ( home, cmds ) =
    ( { model | page = Home home }
    , Cmd.map HomeMsg cmds
    )


stepSignIn : Model -> ( SignIn.Model, Cmd SignIn.Msg ) -> ( Model, Cmd Msg )
stepSignIn model ( signIn, cmds ) =
    ( { model | page = SignIn signIn }
    , Cmd.map SignInMsg cmds
    )



-- ROUTER


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            oneOf
                [ route top
                    (stepSignIn model (SignIn.init model.state url model.key))
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser
