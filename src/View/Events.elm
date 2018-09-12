module View.Events exposing
    ( decodePositionInformation
    , onClickInformation
    , onClickWithPrevent
    , onClickWithStop
    , onInputInformation
    )

import Html.Styled exposing (..)
import Html.Styled.Events exposing (on, preventDefaultOn, stopPropagationOn)
import Json.Decode as Decode exposing (Decoder)
import Type exposing (..)



-- EVENT : WITH POSITION INFORMATIONS


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



-- EVENT : ONCLICK


onClickWithPrevent : msg -> Attribute msg
onClickWithPrevent msg =
    preventDefaultOn
        "click"
        (Decode.map alwaysPreventDefault (Decode.succeed msg))


onClickWithStop : msg -> Attribute msg
onClickWithStop msg =
    stopPropagationOn
        "click"
        (Decode.map alwaysStop (Decode.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


alwaysStop : a -> ( a, Bool )
alwaysStop x =
    ( x, True )
