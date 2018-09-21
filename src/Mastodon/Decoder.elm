module Mastodon.Decoder exposing
    ( accountDecoder
    , appRegistrationDecoder
    , applicationDecoder
    , attachmentDecoder
    , idDecoder
    , mentionDecoder
    , reblogDecoder
    , statusDecoder
    , timelineDecoder
    , statusIdDecoder
    , tagDecoder
    )

import Iso8601
import Json.Decode as Decode exposing (Decoder, bool, int, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Type exposing (Account, AppRegistration, Application, Attachment, AttachmentType(..), Instance, Mention, Reblog(..), Scope, Status, StatusId(..), Tag)
import Url exposing (Url)


urlDecoder : Decoder Url
urlDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case Url.fromString str of
                    Just a ->
                        Decode.succeed a

                    Nothing ->
                        Decode.fail "Unable to decode url"
            )


appRegistrationDecoder : Instance -> Scope -> Decoder AppRegistration
appRegistrationDecoder server scope =
    Decode.succeed AppRegistration
        |> hardcoded server
        |> hardcoded scope
        |> required "client_id" Decode.string
        |> required "client_secret" Decode.string
        |> required "id" idDecoder
        |> required "redirect_uri" urlDecoder


idDecoder : Decoder String
idDecoder =
    Decode.string


accountDecoder : Decoder Account
accountDecoder =
    Decode.succeed Account
        |> required "acct" Decode.string
        |> required "avatar" Decode.string
        |> required "created_at" Iso8601.decoder
        |> required "display_name" Decode.string
        |> required "followers_count" Decode.int
        |> required "following_count" Decode.int
        |> required "header" Decode.string
        |> required "id" idDecoder
        |> required "locked" Decode.bool
        |> required "note" Decode.string
        |> required "statuses_count" Decode.int
        |> required "url" Decode.string
        |> required "username" Decode.string


applicationDecoder : Decoder Application
applicationDecoder =
    Decode.succeed Application
        |> required "name" Decode.string
        |> required "website" (Decode.nullable Decode.string)


statusIdDecoder : Decoder StatusId
statusIdDecoder =
    idDecoder |> Decode.map StatusId


attachmentTypeDecoder : Decoder AttachmentType
attachmentTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "image" ->
                        Decode.succeed Image

                    "video" ->
                        Decode.succeed Video

                    "gifv" ->
                        Decode.succeed GifV

                    _ ->
                        Decode.succeed InvalidAttachment
            )


attachmentDecoder : Decode.Decoder Attachment
attachmentDecoder =
    Decode.succeed Attachment
        |> required "id" idDecoder
        |> required "type" attachmentTypeDecoder
        |> required "url" Decode.string
        |> optional "remote_url" Decode.string ""
        |> required "preview_url" Decode.string
        |> required "text_url" (Decode.nullable Decode.string)


mentionDecoder : Decoder Mention
mentionDecoder =
    Decode.succeed Mention
        |> required "id" idDecoder
        |> required "url" Decode.string
        |> required "username" Decode.string
        |> required "acct" Decode.string

timelineDecoder : Decoder (List Status)
timelineDecoder =
    Decode.list statusDecoder

statusDecoder : Decoder Status
statusDecoder =
    Decode.succeed Status
        |> required "account" accountDecoder
        |> required "application" (Decode.nullable applicationDecoder)
        |> required "content" Decode.string
        |> required "created_at" Decode.string
        |> optional "favourited" (Decode.nullable Decode.bool) Nothing
        |> required "favourites_count" Decode.int
        |> required "id" statusIdDecoder
        |> required "in_reply_to_account_id" (Decode.nullable idDecoder)
        |> required "in_reply_to_id" (Decode.nullable statusIdDecoder)
        |> required "media_attachments" (Decode.list attachmentDecoder)
        |> required "mentions" (Decode.list mentionDecoder)
        |> optional "reblog" (Decode.lazy (\_ -> Decode.nullable reblogDecoder)) Nothing
        |> optional "reblogged" (Decode.nullable Decode.bool) Nothing
        |> required "reblogs_count" Decode.int
        |> required "sensitive" (Decode.nullable Decode.bool)
        |> required "spoiler_text" Decode.string
        |> required "tags" (Decode.list tagDecoder)
        |> required "uri" Decode.string
        |> required "url" (Decode.nullable Decode.string)
        |> required "visibility" Decode.string


tagDecoder : Decode.Decoder Tag
tagDecoder =
    Decode.succeed Tag
        |> required "name" Decode.string
        |> required "url" Decode.string


reblogDecoder : Decoder Reblog
reblogDecoder =
    Decode.map Reblog (Decode.lazy (\_ -> statusDecoder))
