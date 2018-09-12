module Page.Home exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

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
import Toot exposing (Toot, viewToot)
import Type exposing (Account, Auth, Client, Status)



-- MODEL


type alias Model =
    { title : String
    , client : Client
    , timeline : WebData (List Status)
    }


init : Client -> ( Model, Cmd Msg )
init ({ instance, token } as client) =
    ( Model "Welcome on the Fediverse..." client NotAsked
    , homeTimeline instance token (Decode.list statusDecoder) |> Cmd.map TimelineUpdated
    )



-- UPDATE


type Msg
    = NoOp
    | TimelineUpdated (WebData (List Status))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TimelineUpdated timeline ->
            ( { model | timeline = timeline }
            , Cmd.none
            )



-- VIEW


view : Model -> Theme -> Skeleton.Details msg
view model theme =
    { title = "🌎 " ++ model.title ++ " / Tooter"
    , header = []
    , warning = Skeleton.NoProblems
    , kids = [ viewContent theme model ]
    , css =
        [ minHeight (vh 100)
        , paddingTop (rem 2.5)
        ]
    , theme = theme
    }


viewAccount : Theme -> Account -> Html msg
viewAccount theme account =
    div
        [ css
            [ flex3 (int 0) (int 0) theme.layout.sidebarWidth
            , paddingLeft theme.layout.defaultMargin
            ]
        ]
        [ div [ css [ marginBottom (rem 1) ] ] [ accountImage (Just account.avatar) ]
        , div
            [ css [ marginBottom (rem 1) ] ]
            [ div
                [ css
                    [ Theme.headline theme
                    , fontSize (rem 1.34)
                    ]
                ]
                [ text account.display_name ]
            , div
                [ css
                    [ fontWeight (int 500)
                    , fontSize (rem 0.95)
                    , fontStyle italic
                    ]
                ]
                [ text ("@" ++ account.username) ]
            ]
        , div
            [ css [ width (pct 85) ] ]
            [ text account.note ]
        ]


dummyToot : Toot
dummyToot =
    { identifier = "1"
    , content = "@Shaft Je te cache pas que j'ai rendu mon astreinte en partant tout à l'heure et que je suis bien content !"
    , author = { identifier = "@solimanhindy@social.lovetux.net", fullname = "Soliman Hindy", image = Just "https://picsum.photos/200/300?random" }
    }


viewTimeline : Theme -> Account -> Html msg
viewTimeline theme account =
    div
        [ css
            [ flex (int 1)
            , theme.styles.contentSidePadding
            , marginRight (rem 1)
            ]
        ]
        [ viewToot theme dummyToot
        , viewToot theme dummyToot
        , viewToot theme dummyToot
        , viewToot theme dummyToot
        , viewToot theme dummyToot
        , viewToot theme dummyToot
        , viewToot theme dummyToot
        , viewToot theme dummyToot
        , viewToot theme dummyToot
        , viewToot theme dummyToot
        , viewToot theme dummyToot
        , viewToot theme dummyToot
        , viewToot theme dummyToot
        ]


viewAside : Theme -> Account -> Html msg
viewAside theme account =
    div
        [ css
            [ position relative
            , flex3 (int 0) (int 0) theme.layout.secondSidebarWidth
            , paddingLeft theme.layout.smallMargin
            , marginRight (rem 1)
            ]
        ]
        [ div []
            [ div [ css [ Theme.headline theme, fontSize (rem 1.3) ] ] [ text "Who to follow" ]
            , div [ css [ margin2 theme.layout.defaultMargin (rem 0) ] ]
                [ asideLink theme "refresh" "Refresh"
                , asideLink theme "eye" "View all"
                ]
            , accountSuggestionList theme
            ]
        ]


accountSuggestionList : Theme -> Html msg
accountSuggestionList theme =
    div []
        [ accountSuggestion theme (Just "https://picsum.photos/200/300?random")
        , accountSuggestion theme (Just "https://picsum.photos/200/300?random")
        , accountSuggestion theme (Just "https://picsum.photos/200/300?random")
        ]


accountSuggestion : Theme -> Maybe String -> Html msg
accountSuggestion theme maybeImage =
    div [ css [ marginBottom (rem 1.5) ] ]
        [ div
            [ css
                [ displayFlex
                , cursor pointer
                , backgroundColor theme.colors.lightBackground
                , borderRadius (rem 1.25)
                , width (rem 8)
                , height (rem 2)
                , marginBottom (rem 0.25)
                , alignItems center
                , overflow hidden
                , opacity (num 0.6)
                , transition
                    [ Css.Transitions.backgroundColor 180
                    , Css.Transitions.opacity 220
                    ]
                , hover
                    [ backgroundColor theme.colors.major
                    , opacity (num 1.0)
                    ]
                ]
            ]
            [ circularAccountImage maybeImage
            , span [ css [ marginLeft (rem 0.5) ] ] [ icon "person-add" ]
            , span [ css [ marginLeft (rem 0.5) ] ] [ text "Add" ]
            ]
        , div [ css [ fontSize (pct 90), opacity (num 0.6) ] ]
            [ div [ css [ Theme.headline theme ] ] [ text "Dominique Feyer" ]
            , div [ css [ fontSize (pct 90) ] ] [ text "@dfeyer@social.ttree.ch" ]
            ]
        ]


asideLink : Theme -> String -> String -> Html msg
asideLink { colors } iconName label =
    a
        [ href "#"
        , css
            [ display block
            , fontWeight (int 500)
            , color colors.accent
            , textDecoration none
            ]
        ]
        [ icon iconName, span [ css [ marginLeft (rem 0.5) ] ] [ text label ] ]


viewContent : Theme -> Model -> Html msg
viewContent theme { client } =
    div
        [ css
            [ displayFlex
            ]
        ]
        [ viewAccount theme client.account
        , viewTimeline theme client.account
        , viewAside theme client.account
        ]
