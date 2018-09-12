module Image exposing (accountImage, circularAccountImage, smallAccountImage)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)


type alias Dimension units =
    ExplicitLength units


type alias Radius units =
    ExplicitLength units


type alias AccountImage =
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


accountImage : AccountImage -> Html msg
accountImage maybeImage =
    squaredImage
        (rem 6)
        (rem 0.5)
        maybeImage


smallAccountImage : AccountImage -> Html msg
smallAccountImage maybeImage =
    squaredImage
        (rem 3.5)
        (rem 0.5)
        maybeImage


circularAccountImage : AccountImage -> Html msg
circularAccountImage maybeImage =
    squaredImage
        (rem 2)
        (pct 50)
        maybeImage
