module Account exposing (decoder, encoder)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Type exposing (Account)


encoder : Account -> Encode.Value
encoder account =
    Encode.object
        [ ( "acct", Encode.string account.acct )
        , ( "avatar", Encode.string account.avatar )
        , ( "created_at", Iso8601.fromTime account.created_at |> Encode.string )
        , ( "display_name", Encode.string account.display_name )
        , ( "followers_count", Encode.int account.followers_count )
        , ( "following_count", Encode.int account.following_count )
        , ( "header", Encode.string account.header )
        , ( "id", Encode.string account.id )
        , ( "locked", Encode.bool account.locked )
        , ( "note", Encode.string account.note )
        , ( "statuses_count", Encode.int account.statuses_count )
        , ( "url", Encode.string account.url )
        , ( "username", Encode.string account.username )
        ]


decoder : Decoder Account
decoder =
    Decode.succeed Account
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
