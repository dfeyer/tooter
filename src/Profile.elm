module Profile exposing (decoder, encoder)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Type exposing (Profile)


encoder : Profile -> Encode.Value
encoder profile =
    Encode.object
        [ ( "acct", Encode.string profile.acct )
        , ( "avatar", Encode.string profile.avatar )
        , ( "created_at", Iso8601.fromTime profile.created_at |> Encode.string )
        , ( "display_name", Encode.string profile.display_name )
        , ( "followers_count", Encode.int profile.followers_count )
        , ( "following_count", Encode.int profile.following_count )
        , ( "header", Encode.string profile.header )
        , ( "id", Encode.string profile.id )
        , ( "locked", Encode.bool profile.locked )
        , ( "note", Encode.string profile.note )
        , ( "statuses_count", Encode.int profile.statuses_count )
        , ( "url", Encode.string profile.url )
        , ( "username", Encode.string profile.username )
        ]


decoder : Decoder Profile
decoder =
    Decode.succeed Profile
        |> required "acct" Decode.string
        |> required "avatar" Decode.string
        |> required "created_at" Iso8601.decoder
        |> required "display_name" Decode.string
        |> required "followers_count" Decode.int
        |> required "following_count" Decode.int
        |> required "header" Decode.string
        |> required "id" Decode.string
        |> required "locked" Decode.bool
        |> required "note" Decode.string
        |> required "statuses_count" Decode.int
        |> required "url" Decode.string
        |> required "username" Decode.string
