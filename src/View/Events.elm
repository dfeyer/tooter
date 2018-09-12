module View.Events exposing
    ( decodePositionInformation
    , onClickInformation
    , onClickWithPrevent
    , onClickWithPreventAndStop
    , onClickWithStop
    , onInputInformation
    )

import Html.Styled exposing (..)
import Html.Styled.Events exposing (on, preventDefaultOn)
import Json.Decode as Decode exposing (Decoder)
import Type exposing (..)


onClickInformation : (InputInformation -> msg) -> Attribute msg
onClickInformation msg =
    on "mouseup" (Decode.map msg decodePositionInformation)


onInputInformation : (InputInformation -> msg) -> Attribute msg
onInputInformation msg =
    on "input" (Decode.map msg decodePositionInformation)


decodePositionInformation : Decoder InputInformation
decodePositionInformation =
    Decode.map2 InputInformation
        (Decode.at [ "target", "value" ] Decode.string)
        (Decode.at [ "target", "selectionStart" ] Decode.int)


onClickWithPreventAndStop : msg -> Attribute msg
onClickWithPreventAndStop msg =
    preventDefaultOn
        "click"
        (Decode.map alwaysPreventDefault (Decode.succeed msg))


onClickWithPrevent : msg -> Attribute msg
onClickWithPrevent msg =
    preventDefaultOn
        "click"
        (Decode.map alwaysPreventDefault (Decode.succeed msg))


onClickWithStop : msg -> Attribute msg
onClickWithStop msg =
    preventDefaultOn
        "click"
        (Decode.map alwaysPreventDefault (Decode.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )
