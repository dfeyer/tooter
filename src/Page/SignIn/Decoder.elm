module Page.SignIn.Decoder exposing (accountDecoder)

import Json.Decode as Json exposing (Decoder, bool, int, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Type exposing (Account)
import Iso8601

accountDecoder : Decoder Account
accountDecoder =
    Json.succeed Account
        |> required "acct" string
        |> required "avatar" string
        |> required "created_at" Iso8601.decoder
        |> required "display_name" string
        |> required "followers_count" int
        |> required "following_count" int
        |> required "header" string
        |> required "id" string
        |> required "locked" bool
        |> required "note" string
        |> required "statuses_count" int
        |> required "url" string
        |> required "username" string
