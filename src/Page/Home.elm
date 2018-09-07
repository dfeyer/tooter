module Page.Home exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Css exposing (..)
import Html.Styled exposing (..)
import Skeleton exposing (Theme)



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
    { title = model.title
    , header = []
    , warning = Skeleton.NoProblems
    , kids = [ viewContent model.title ]
    , css =
        [ minHeight (vh 100)
        , paddingTop (rem 1.5)
        ]
    , theme = theme
    }


viewContent : String -> Html msg
viewContent title =
    div []
        [ text title
        ]
