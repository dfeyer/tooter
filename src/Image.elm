module Image exposing (circularProfileImage, profileImage, smallProfileImage)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)


type alias Dimension units =
    ExplicitLength units


type alias Radius units =
    ExplicitLength units


type alias ProfileImage =
    Maybe String


squaredImage : Dimension a -> Radius b -> Maybe String -> Html msg
squaredImage dimension radius maybeImage =
    let
        style =
            [ width dimension, height dimension, borderRadius radius, property "object-fit" "cover" ]
    in
    case maybeImage of
        Just imageUrl ->
            img [ src imageUrl, css style ] []

        Nothing ->
            div [ css style ] []


profileImage : ProfileImage -> Html msg
profileImage maybeImage =
    squaredImage
        (rem 6)
        (rem 0.5)
        maybeImage


smallProfileImage : ProfileImage -> Html msg
smallProfileImage maybeImage =
    squaredImage
        (rem 3.5)
        (rem 0.5)
        maybeImage


circularProfileImage : ProfileImage -> Html msg
circularProfileImage maybeImage =
    squaredImage
        (rem 2)
        (pct 50)
        maybeImage
