module Mastodon.Encoder exposing (accountEncoder, appRegistrationEncoder)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Type exposing (Account, AppRegistration, Scope)
import Url exposing (Url)


appRegistrationEncoder : String -> Url -> Scope -> String -> Encode.Value
appRegistrationEncoder clientName redirectUris scope website =
    Encode.object
        [ ( "client_name", Encode.string clientName )
        , ( "redirect_uris", Encode.string (Url.toString redirectUris) )
        , ( "scopes", Encode.string scope )
        , ( "website", Encode.string website )
        ]


accountEncoder : Account -> Encode.Value
accountEncoder account =
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
