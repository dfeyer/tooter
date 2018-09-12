module Request.Timeline exposing (homeTimeline)

import Http
import Json.Decode exposing (Decoder)
import Mastodon.Url as Api
import OAuth exposing (Token)
import RemoteData exposing (WebData)
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


homeTimeline : Instance -> Token -> Decoder a -> Cmd (WebData a)
homeTimeline instance token decoder =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , withCredentials = False
        , url = Url.toString (url instance Api.homeTimeline)
        , expect = Http.expectJson decoder
        , timeout = Nothing
        }
        |> RemoteData.sendRequest
