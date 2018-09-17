module Mastodon.OAuth exposing (initOAuthConfiguration)

import Mastodon.Decoder exposing (accountDecoder)
import Mastodon.Url as Api
import Type exposing (OAuthConfiguration)
import Url exposing (Protocol(..), Url)


initOAuthConfiguration : Url -> OAuthConfiguration
initOAuthConfiguration url =
    let
        defaultHttpsUrl =
            { protocol = Https
            , host = ""
            , port_ = Nothing
            , path = ""
            , query = Nothing
            , fragment = Nothing
            }
    in
    { authorizationEndpoint = { defaultHttpsUrl | path = Api.oauthAuthorize }
    , tokenEndpoint = { defaultHttpsUrl | path = Api.oauthToken }
    , accountEndpoint = { defaultHttpsUrl | path = Api.userAccount }
    , scope = [ "read", "write", "follow" ]
    , accountDecoder = accountDecoder
    , redirectUri = { url | query = Nothing, fragment = Nothing }
    }
