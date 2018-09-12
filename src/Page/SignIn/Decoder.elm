module Page.SignIn.Decoder exposing (profileDecoder)

import Json.Decode as Json exposing (Decoder, bool, int, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Type exposing (Profile)
import Iso8601

profileDecoder : Decoder Profile
profileDecoder =
    Json.succeed Profile
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
