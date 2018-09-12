module Token exposing (decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import OAuth exposing (Token)


decoder : Decoder Token
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case OAuth.tokenFromString str of
                    Just token ->
                        Decode.succeed token

                    Nothing ->
                        Decode.fail "Unable to decode token"
            )
