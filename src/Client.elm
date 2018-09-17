module Client exposing (resume, save)

import Base64
import Decoder exposing (tokenDecoder)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Mastodon.Decoder exposing (accountDecoder)
import Mastodon.Encoder exposing (accountEncoder)
import OAuth
import Ports
import Type exposing (..)
import Utility exposing (toJson)


resume : String -> Result Error (Maybe Client)
resume clients =
    case Base64.decode clients of
        Ok clients_ ->
            case Decode.decodeString clientListDecoder clients_ of
                Ok list ->
                    -- This a workaround to get the first client only
                    -- we can change this later when we implement
                    -- multiple instance support
                    Ok (list |> List.head)

                Err _ ->
                    Err (InvalidToken "Unable to parse stored account and token")

        Err _ ->
            Err (InvalidToken "Unable to decode stored account and token")


save : List Client -> Cmd msg
save clients =
    clients
        |> List.map clientEncoder
        |> Encode.list identity
        |> toJson
        |> Base64.encode
        |> Ports.saveClients


clientEncoder : Client -> Encode.Value
clientEncoder client =
    Encode.object
        [ ( "instance", Encode.string client.instance )
        , ( "token", OAuth.tokenToString client.token |> Encode.string )
        , ( "account", accountEncoder client.account )
        ]


clientDecoder : Decoder Client
clientDecoder =
    Decode.succeed Client
        |> required "instance" Decode.string
        |> required "token" tokenDecoder
        |> required "account" accountDecoder


clientListDecoder : Decoder (List Client)
clientListDecoder =
    Decode.list clientDecoder
