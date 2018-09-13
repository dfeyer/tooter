module Page.Home exposing
    ( Model
    , Msg
    , TimelineMode(..)
    , init
    , update
    , view
    )

import Browser.Navigation exposing (Key, pushUrl)
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Icon exposing (icon)
import Image exposing (accountImage, circularAccountImage)
import Json.Decode as Decode
import Mastodon.Decoder exposing (statusDecoder)
import OAuth exposing (Token)
import RemoteData exposing (RemoteData(..), WebData)
import Request.Timeline exposing (homeTimeline)
import Skeleton
import Theme exposing (Theme)
import Type exposing (Account, Auth, Client, Status)
import View.Account as Account
import View.Status exposing (viewStatus)
import View.WhoToFollow as WhoToFollow
import View.Zone as Zone



-- MODEL


type alias Model =
    { title : String
    , key : Key
    , timelineMode : TimelineMode
    , client : Client
    , timeline : Timeline
    }


type TimelineMode
    = HomeTimeline
    | LocalTimeline
    | FederatedTimeline


type alias Timeline =
    WebData (List Status)


init : Key -> TimelineMode -> Client -> ( Model, Cmd Msg )
init key timelineMode ({ instance, token } as client) =
    ( Model "Welcome on the Fediverse..." key timelineMode client NotAsked
    , homeTimeline instance token (Decode.list statusDecoder) |> Cmd.map TimelineUpdated
    )



-- UPDATE


type Msg
    = NoOp
    | Navigate String
    | TimelineUpdated (WebData (List Status))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Navigate href ->
            ( model, pushUrl model.key href )

        TimelineUpdated timeline ->
            ( { model | timeline = timeline }
            , Cmd.none
            )



-- VIEW


view : Model -> Theme -> Skeleton.Details Msg
view model theme =
    { title = "🌎 " ++ model.title ++ " / Tooter"
    , header = []
    , warning = Skeleton.NoProblems
    , kids = [ viewContent theme model ]
    , sidebar = []
    , aside = []
    , css =
        [ minHeight (vh 100)
        , paddingTop (rem 2.5)
        ]
    , theme = theme
    }


viewTimeline : Theme -> Timeline -> List (Html Msg)
viewTimeline theme timeline =
    [ case timeline of
        NotAsked ->
            div [] [ text "Initialising..." ]

        Loading ->
            div [] [ text "Loading..." ]

        Failure _ ->
            div [] [ text "Oups... something bad happens. We are sorry, but not perfect. Maybe try again?" ]

        Success list ->
            div [] (List.map (viewStatus theme) list)
    ]


viewContent : Theme -> Model -> Html Msg
viewContent theme { client, timeline } =
    div
        [ css
            [ displayFlex
            ]
        ]
        [ Zone.sidebar theme (Account.view theme client.account)
        , Zone.mainArea theme (viewTimeline theme timeline)
        , Zone.aside theme (WhoToFollow.view theme client.account)
        ]
