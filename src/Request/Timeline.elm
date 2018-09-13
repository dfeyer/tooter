module Request.Timeline exposing (homeTimeline, accountTimeline, publicTimeline, favouriteTimeline)

import Http
import Json.Decode exposing (Decoder)
import Mastodon.Url as Api
import OAuth exposing (Token)
import RemoteData exposing (WebData)
import Type exposing (Account, Instance)
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



accountTimeline : Instance -> Token -> Account -> Decoder a -> Cmd (WebData a)
accountTimeline instance token {id} decoder =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , withCredentials = False
        , url = Url.toString (url instance (Api.accountTimeline id))
        , expect = Http.expectJson decoder
        , timeout = Nothing
        }
        |> RemoteData.sendRequest



publicTimeline : Instance -> Token -> Decoder a -> Cmd (WebData a)
publicTimeline instance token decoder =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , withCredentials = False
        , url = Url.toString (url instance Api.publicTimeline)
        , expect = Http.expectJson decoder
        , timeout = Nothing
        }
        |> RemoteData.sendRequest



favouriteTimeline : Instance -> Token -> Decoder a -> Cmd (WebData a)
favouriteTimeline instance token decoder =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , withCredentials = False
        , url = Url.toString (url instance Api.favouriteTimeline)
        , expect = Http.expectJson decoder
        , timeout = Nothing
        }
        |> RemoteData.sendRequest
