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
import Html.Styled.Lazy exposing (lazy2)
import Icon exposing (icon)
import Image exposing (accountImage, circularAccountImage)
import Json.Decode as Decode
import Mastodon.Decoder exposing (timelineDecoder)
import Mastodon.View.Status as Status
import OAuth exposing (Token)
import RemoteData exposing (RemoteData(..), WebData)
import Request.Timeline exposing (..)
import Skeleton exposing (Segment(..))
import Theme exposing (Theme)
import Type exposing (Account, Auth, Client, Status, Timeline)
import View.Account as Account
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
    | FavouritesTimeline


init : Key -> TimelineMode -> Client -> ( Model, Cmd Msg )
init key timelineMode ({ instance, token } as client) =
    ( Model "Welcome on the Fediverse..." key timelineMode client NotAsked
    , initCmd timelineMode client
    )

initCmd : TimelineMode -> Client -> Cmd Msg
initCmd mode { instance, token, account } =
    case mode of
        HomeTimeline ->
            Cmd.map AccountTimelineUpdated <|
                accountTimeline instance token account timelineDecoder

        LocalTimeline ->
            Cmd.map PublicTimelineUpdated <|
                publicTimeline instance token timelineDecoder

        FederatedTimeline ->
            Cmd.map FederatedTimelineUpdated <|
                homeTimeline instance token timelineDecoder

        FavouritesTimeline ->
            Cmd.map FavouritesTimelineUpdated <|
                favouriteTimeline instance token timelineDecoder



-- UPDATE


type Msg
    = NoOp
    | Navigate String
    | AccountTimelineUpdated (WebData (List Status))
    | PublicTimelineUpdated (WebData (List Status))
    | FederatedTimelineUpdated (WebData (List Status))
    | FavouritesTimelineUpdated (WebData (List Status))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Navigate href ->
            ( model, pushUrl model.key href )

        AccountTimelineUpdated timeline ->
            ( { model | timeline = timeline }
            , Cmd.none
            )

        PublicTimelineUpdated timeline ->
            ( { model | timeline = timeline }
            , Cmd.none
            )

        FederatedTimelineUpdated timeline ->
            ( { model | timeline = timeline }
            , Cmd.none
            )

        FavouritesTimelineUpdated timeline ->
            ( { model | timeline = timeline }
            , Cmd.none
            )



-- VIEW


view : Model -> Theme -> Skeleton.Details Msg
view model theme =
    { title = "🌎 " ++ model.title ++ " / Tooter"
    , navigation = Nothing
    , warning = Skeleton.NoProblems
    , kids = [ viewContent theme model ]
    , sidebar = []
    , aside = []
    , styles =
        [ minHeight (vh 100)
        , paddingTop (rem 2.5)
        ]
    , theme = theme
    }


viewContent : Theme -> Model -> Html Msg
viewContent theme { client, timeline } =
    div
        [ css
            [ displayFlex
            ]
        ]
        [ lazy2 Zone.sidebar theme (Account.view theme client.account)
        , lazy2 Zone.mainArea theme (Status.viewTimeline theme timeline)
        , lazy2 Zone.aside theme (WhoToFollow.view theme client.account)
        ]
