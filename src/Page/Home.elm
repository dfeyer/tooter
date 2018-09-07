module Page.Home exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Icon exposing (icon)
import Skeleton
import Theme exposing (Theme)



-- MODEL


type alias Model =
    { title : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "Home", Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Theme -> Skeleton.Details msg
view model theme =
    { title = "🌎 Home / Tooter"
    , header = []
    , warning = Skeleton.NoProblems
    , kids = [ viewContent theme model.title ]
    , css =
        [ minHeight (vh 100)
        , paddingTop (rem 2.5)
        ]
    , theme = theme
    }


image : List Style -> Maybe String -> Html msg
image style maybeImage =
    case maybeImage of
        Just imageUrl ->
            img [ src imageUrl, css style ] []

        Nothing ->
            div [ css style ] []


profileImage : Maybe String -> Html msg
profileImage maybeImage =
    image
        [ width (rem 6), height (rem 6), borderRadius (rem 0.5), property "object-fit" "cover" ]
        maybeImage


smallProfileImage : Maybe String -> Html msg
smallProfileImage maybeImage =
    image
        [ width (rem 3.5), height (rem 3.5), borderRadius (rem 0.5), property "object-fit" "cover" ]
        maybeImage


viewProfile : Theme -> Html msg
viewProfile theme =
    div
        [ css
            [ flex3 (int 0) (int 0) theme.layout.sidebarWidth
            , paddingLeft theme.layout.smallMargin
            ]
        ]
        [ div [ css [ marginBottom (rem 1) ] ] [ profileImage (Just "https://picsum.photos/200/300") ]
        , div
            [ css [ marginBottom (rem 1) ] ]
            [ div
                [ css
                    [ theme.styles.headlineFontFamily
                    , theme.styles.headlineFontWeight
                    , fontSize (rem 1.34)
                    ]
                ]
                [ text "Dominique Feyer" ]
            , div
                [ css
                    [ fontWeight (int 500)
                    , fontSize (rem 0.95)
                    , fontStyle italic
                    ]
                ]
                [ text "@dfeyer@social.ttree.ch" ]
            ]
        , div
            [ css [ width (pct 85) ] ]
            [ text "Father, co-founder of ttree.ch and medialib.tv, content management expert, devops fanboy, open source contributor, Neos CMS &amp; Flow Framework team member" ]
        ]


type alias Account =
    { identifier : String
    , fullname : String
    , image : Maybe String
    }


type alias Toot =
    { identifier : String
    , content : String
    , author : Account
    }


viewToot : Theme -> Toot -> Html msg
viewToot theme {author, content} =
    div
        [ css [ displayFlex, marginBottom (rem 2) ] ]
        [ div [ css [ marginRight (rem 1) ] ] [ smallProfileImage author.image ]
        , div []
            [ tootAccount theme author
            , div [ css [ marginTop (rem 0.25) ] ] [ text content ]
            , tootBar theme
            ]
        ]

tootAccount : Theme -> Account -> Html msg
tootAccount theme { fullname, identifier } =
    div []
        [ span
            [ css
                [ theme.styles.headlineFontFamily
                , theme.styles.headlineFontWeight
                ]
            ]
            [ text fullname ]
        , span [ css [ opacity (num 0.2) ] ] [ text " | " ]
        , span
            [ css
                [ fontWeight (int 500)
                , fontSize (rem 0.95)
                , fontStyle italic
                ]
            ]
            [ text identifier ]
        ]

tootBar : Theme -> Html msg
tootBar theme =
    div
        [ css [ displayFlex, fontSize (rem 1.2), marginTop (rem 0.35), justifyContent end ] ]
        [ tootBarLink theme "Send a resonse..." "return-left"
        , tootBarLink theme "Boost" "refresh"
        , tootBarLink theme "Add to your favorits" "star-outline"
        ]


tootBarLink : Theme -> String -> String -> Html msg
tootBarLink theme label iconName =
    a [ href "#", css [ display block, color theme.colors.darkText, marginLeft (rem 1.5), opacity (num 0.5) ] ] [ icon iconName ]


dummyToot : Toot
dummyToot =
    { identifier = "1"
    , content = "@Shaft Je te cache pas que j'ai rendu mon astreinte en partant tout à l'heure et que je suis bien content !"
    , author = { identifier = "@solimanhindy@social.lovetux.net", fullname = "Soliman Hindy", image = Just "https://picsum.photos/200/300" }
    }


viewTimeline : Theme -> Html msg
viewTimeline theme =
    div
        [ css
            [ flex (int 1)
            , theme.styles.contentSidePadding
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


viewAside : Theme -> Html msg
viewAside theme =
    div
        [ css
            [ flex3 (int 0) (int 0) theme.layout.secondSidebarWidth
            , paddingLeft theme.layout.smallMargin
            ]
        ]
        [ text "aside" ]


viewContent : Theme -> String -> Html msg
viewContent theme title =
    div
        [ css
            [ displayFlex
            ]
        ]
        [ viewProfile theme
        , viewTimeline theme
        , viewAside theme
        ]
