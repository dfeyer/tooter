module Page.Inbox exposing
    ( Model
    , Msg
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
import View.WhoToFollow as WhoToFollow
import View.Zone as Zone



-- MODEL


type alias Model =
    { title : String
    , key : Key
    , client : Client
    }


init : Key -> Client -> ( Model, Cmd Msg )
init key ({ instance, token } as client) =
    ( Model "Notifications" key client
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



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
viewContent theme { client } =
    div
        [ css
            [ displayFlex
            ]
        ]
        [ Zone.sidebar theme (Account.view theme client.account)
        , Zone.mainArea theme [ div [] [ text "Notification..." ] ]
        , Zone.aside theme (WhoToFollow.view theme client.account)
        ]
