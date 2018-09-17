module Decoder exposing (tokenDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Mastodon.Decoder exposing (accountDecoder)
import OAuth exposing (Token)
import Type exposing (Client)


tokenDecoder : Decoder Token
tokenDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case OAuth.tokenFromString str of
                    Just token ->
                        Decode.succeed token

                    Nothing ->
                        Decode.fail "Unable to decode token"
            )
