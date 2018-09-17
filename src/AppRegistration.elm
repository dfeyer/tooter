module AppRegistration exposing (delete, resume, save)

import Base64
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import Mastodon.Decoder exposing (idDecoder)
import Ports
import Type exposing (..)
import Url exposing (Url)
import Utility exposing (toJson)


resume : Url -> String -> Result Error (Maybe AppRegistration)
resume url r =
    if r == "" then
        Ok Nothing

    else
        case Base64.decode r of
            Ok r_ ->
                case
                    Decode.decodeString (registrationDecoder url) r_
                of
                    Ok a ->
                        Ok (Just a)

                    Err _ ->
                        Err (InvalidAppRegistration "Unable to parse stored app registration")

            Err _ ->
                Err (InvalidAppRegistration "Unable to decode stored app registration")


save : AppRegistration -> Cmd msg
save registration =
    registrationEncoder registration
        |> toJson
        |> Base64.encode
        |> Ports.saveRegistration


delete : Cmd msg
delete =
    Ports.deleteRegistration ""


registrationDecoder : Url -> Decoder AppRegistration
registrationDecoder url =
    Decode.succeed AppRegistration
        |> required "instance" Decode.string
        |> required "scope" Decode.string
        |> required "clientId" Decode.string
        |> required "clientSecret" Decode.string
        |> required "id" idDecoder
        |> hardcoded url


registrationEncoder : AppRegistration -> Encode.Value
registrationEncoder registration =
    Encode.object
        [ ( "instance", Encode.string registration.instance )
        , ( "scope", Encode.string registration.scope )
        , ( "clientId", Encode.string registration.clientId )
        , ( "clientSecret", Encode.string registration.clientSecret )
        , ( "id", Encode.string registration.id )
        ]
