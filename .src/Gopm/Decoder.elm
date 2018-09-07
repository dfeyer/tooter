module Gopm.Decoder exposing (loadDirectoryDecoder)

import Time
import Gopm.Model exposing (..)
import Json.Decode as Decode exposing (Decoder, andThen, bool, float, int, nullable, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Iso8601


loadDirectoryDecoder : Decoder DirectoryListing
loadDirectoryDecoder =
    Decode.succeed DirectoryListing
        |> required "meta" loadDirectoryMetaDecoder
        |> required "objects" (Decode.list documentDecoder)


loadDirectoryMetaDecoder : Decoder CurrentFolder
loadDirectoryMetaDecoder =
    Decode.succeed CurrentFolder
        |> required "count" int
        |> required "path" string
        |> required "rootline" rootlineDecoder



-- DECODER : ROOTLINE


rootlineDecoder : Decoder (List RootlineSegment)
rootlineDecoder =
    Decode.list rootlineSegmentDecoder


rootlineSegmentDecoder : Decoder RootlineSegment
rootlineSegmentDecoder =
    Decode.succeed RootlineSegment
        |> required "name" string
        |> required "path" string
        |> required "icon" string



-- DECODER : ECM METADATA


ecmMetaDataDecoder : Decoder EcmMetadata
ecmMetaDataDecoder =
    Decode.succeed EcmMetadata
        |> required "ecmpub_name" string
        |> required "ecmpub_lastUpdate" (nullable date)
        |> required "ecmpub_folder" (nullable string)
        |> required "ecmpub_summary" (nullable string)



-- DECODER : GOPM METADATA


gopmMetaDataDecoder : Decoder GopmMetadata
gopmMetaDataDecoder =
    Decode.succeed GopmMetadata
        |> required "gopm_annee_referencement" (nullable int)
        |> custom lineDecoder
        |> custom stationDecoder


lineDecoder : Decoder Line
lineDecoder =
    Decode.succeed Line
        |> required "gopm_exploitant_ligne" (nullable string)
        |> required "gopm_nom_ligne" (nullable string)
        |> required "gopm_type_ligne" (nullable string)


stationDecoder : Decoder Station
stationDecoder =
    Decode.succeed Station
        |> required "gopm_type_arret" (nullable string)
        |> required "gopm_genre_arret" (nullable string)
        |> required "gopm_nom_arret" (nullable string)



-- DECODER :: DOCUMENT


documentDecoder : Decoder Document
documentDecoder =
    Decode.succeed Document
        |> required "__priority" int
        |> required "id" string
        |> required "key" string
        |> required "type" documentType
        |> required "_isDownloadeable" bool
        |> required "_downloadUri" (nullable string)
        |> required "name" string
        |> required "_icon" string
        |> required "description" (nullable string)
        |> required "version" (nullable string)
        |> required "path" string
        |> required "creation_date" date
        |> required "last_modification_date" date
        |> custom ecmMetaDataDecoder
        |> custom gopmMetaDataDecoder


documentType : Decoder DocumentType
documentType =
    string
        |> andThen (fromResult << parseDocumentType)


parseDocumentType : String -> Result String DocumentType
parseDocumentType t =
    case t of
        "document" ->
            Ok File

        "folder" ->
            Ok Folder

        _ ->
            Err "Invalid document type"



-- DECODER :: INTERNAL HELPER


date : Decoder Time.Posix
date =
    string
        |> andThen (fromResult << Iso8601.toTime)


fromResult : Result b a -> Decoder a
fromResult result =
    case result of
        Ok a ->
            Decode.succeed a

        Err message ->
            Decode.fail "Fail"