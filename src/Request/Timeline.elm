module Request.Timeline exposing (homeTimeline)

import Http
import Json.Decode exposing (Decoder)
import Mastodon.Url exposing (homeTimeline)
import OAuth exposing (Token)
import RemoteData
import Type exposing (Instance)
import Url exposing (Protocol(..), Url)


type alias Path =
    String


url : Instance -> Path -> Url
url instance path =
    { protocol = Https
    , host = instance
    , path = path
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


homeTimeline : Instance -> Token -> (a -> msg) -> Decoder a -> Cmd msg
homeTimeline instance token msg decoder =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , withCredentials = False
        , url = Url.toString (url instance homeTimeline)
        , expect = Http.expectJson decoder
        , timeout = Nothing
        }
        |> RemoteData.sendRequest
        |> Cmd.map msg
