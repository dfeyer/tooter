module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Navigation as Nav
import Html
import Html.Styled exposing (Html, button, div, text)
import Page.Home as Home
import Page.Problem as Problem
import Skeleton
import Theme exposing (Palette, Theme, createTheme)
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)


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
    }


type Page
    = NotFound
    | Home Home.Model



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



-- INIT


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    stepUrl url
        { key = key
        , page = NotFound
        , theme = createTheme
        }



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg


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


stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
stepHome model ( home, cmds ) =
    ( { model | page = Home home }
    , Cmd.map HomeMsg cmds
    )



-- ROUTER


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
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


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser
