module Gopm.Main exposing (..)

import Gopm.Decoder exposing (loadDirectoryDecoder)
import Gopm.Model exposing (..)
import Gopm.Util exposing (formatDate, formatName)
import Html exposing (..)
import Html.Attributes exposing (class, colspan, href)
import Html.Events exposing (onClick)
import Http
import Debug exposing (toString)
import Browser
import Browser.Navigation as Nav
import RemoteData exposing (RemoteData(..), WebData)
import String.Extra exposing (replace)


main : Program Never Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = UrlChange
        , onUrlChange = UrlChange
        }



-- MODEL


type alias Model =
    { history : List Nav.Key
    , content : BrowserContent
    }


init : Nav.Key -> ( Model, Cmd Msg )
init location =
    ( Model [ location ] ContentNotAsked
    , loadDirectoryByHash location
    )



-- UPDATE


type Msg
    = UrlChange Nav.Key
    | LoadDirectory String
    | HandleDirectoryLoaded (WebData DirectoryListing)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadDirectory path ->
            ( model
            , loadDirectory path
            )

        HandleDirectoryLoaded response ->
            ( { model | content = DocumentList response }
            , Cmd.none
            )

        UrlChange location ->
            ( { model | history = location :: model.history }
            , loadDirectoryByHash location
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "af" ]
        [ viewMain model
        , viewHistory model
        ]


viewLocation : Nav.Key -> Html msg
viewLocation location =
    li [] [ text (toString location) ]



-- VIEW :: HISTORY


viewHistory : Model -> Html msg
viewHistory model =
    div []
        [ h1 [] [ text "History" ]
        , ul [] (List.map viewLocation model.history)
        ]



-- VIEW :: DIRECTORY LISTING


viewMain : Model -> Html Msg
viewMain model =
    case model.content of
        DocumentList list ->
            viewDocumentList list

        SingleDocument document ->
            viewSingleDocument document

        ContentNotAsked ->
            div [] [ text "Loading" ]



-- VIEW :: DOCUMENT LIST


viewDocumentList : WebData DirectoryListing -> Html Msg
viewDocumentList list =
    case list of
        NotAsked ->
            div [] [ text "En cours de chargement" ]

        Loading ->
            div [] [ text "En cours de chargement" ]

        Failure err ->
            div [] [ text (toString err) ]

        Success content ->
            viewDocumentListTable (List.map viewDocumentRow content.objects) (viewRootline content.metadata.rootline) content.metadata.count


viewDocumentRow : Document -> Html Msg
viewDocumentRow ({ type_, icon, path, name, creationDate, lastModificationDate } as document) =
    let
        td =
            viewLinkedColumn path

        buttons =
            viewColumn
    in
    case type_ of
        File ->
            tr [ class (af "object") ]
                [ td "name" [ linkWithIcon icon path (formatName name) ]
                , td "created" [ text (formatDate creationDate) ]
                , td "updated" [ text (formatDate lastModificationDate) ]
                , buttons "download" [ viewDocumentDownloadButton document ]
                ]

        Folder ->
            tr [ class (af "object") ]
                [ td "name" [ linkWithIcon icon path (formatName name) ]
                , td "created" [ text (formatDate creationDate) ]
                , td "updated" [ text (formatDate lastModificationDate) ]
                , buttons "download" [ viewDocumentDownloadButton document ]
                ]


viewLinkedColumn : String -> String -> List (Html Msg) -> Html Msg
viewLinkedColumn path part label =
    td [ class (af "object-" ++ part), onClick (LoadDirectory path) ] label


viewColumn : String -> List (Html Msg) -> Html Msg
viewColumn part label =
    td [ class (af "object-" ++ part) ] label


viewDocumentDownloadButton : Document -> Html msg
viewDocumentDownloadButton { uri } =
    case uri of
        Nothing ->
            span [] []

        Just u ->
            a [ href u ] [ awesomeIcon "download" ]


viewDocumentListTable : List (Html msg) -> Html msg -> Int -> Html msg
viewDocumentListTable content rootline count =
    div []
        [ rootline
        , table [ class ("table " ++ af "content") ]
            [ thead []
                [ tr [ class (af "object-header") ]
                    [ th [ class (af "object-name") ] [ text "Nom" ]
                    , th [ class (af "object-created") ] [ text "Création" ]
                    , th [ class (af "object-updated") ] [ text "Modification" ]
                    , th [ class (af "object-download") ] []
                    ]
                ]
            , tbody [] content
            , tfoot []
                [ tr [ class (af "object-footer") ]
                    [ td [ class (af "object-footer__content"), colspan 4 ] [ text ("Le dossier courant contiens " ++ toString count ++ " élément(s)") ]
                    ]
                ]
            ]
        ]



-- VIEW :: SINGLE DOCUMENT


viewSingleDocument : WebData Document -> Html msg
viewSingleDocument document =
    case document of
        NotAsked ->
            div [] [ text "En cours de chargement" ]

        Loading ->
            div [] [ text "En cours de chargement" ]

        Failure err ->
            div [] []

        Success content ->
            div [] [ text "Single Document" ]



-- VIEW :: ROOTLINE


viewRootline : List RootlineSegment -> Html msg
viewRootline rootline =
    ul [ class (af "breadcrumb") ] (List.map viewRootlineSegment rootline)


viewRootlineSegment : RootlineSegment -> Html msg
viewRootlineSegment { icon, path, name } =
    li [ class (af "breadcrumb-item") ] [ linkWithIcon icon path (formatName name) ]



-- VIEW :: HELPERS


awesomeIcon : String -> Html msg
awesomeIcon t =
    i [ class ("fas fa-" ++ replace "-o" "" t) ] []


linkWithIcon : String -> String -> String -> Html msg
linkWithIcon i p l =
    a [ href ("#" ++ p) ]
        [ span [ class (af "linkicon-icon") ] [ awesomeIcon i ]
        , span [ class (af "linkicon-label") ] [ text l ]
        ]



-- REQUEST


loadDirectory : String -> Cmd Msg
loadDirectory path =
    let
        url =
            "https://www.vd.ch/index.php?eID=vd_alfresco_browser&configuration=1202227&path="
    in
    Http.get (url ++ path) loadDirectoryDecoder
        |> RemoteData.sendRequest
        |> Cmd.map HandleDirectoryLoaded


loadDirectoryByHash : Nav.Key -> Cmd Msg
loadDirectoryByHash key =
    case
        String.split "#" (toString key)
            |> List.reverse
            |> List.head
    of
        Just path ->
            loadDirectory path

        Nothing ->
            loadDirectory ""


af : String -> String
af s =
    "af__" ++ s
