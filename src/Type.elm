module Type exposing (Auth, Client, OAuthConfiguration, Profile, Server, initAuth, resumeAuth)

import Browser.Navigation as Nav exposing (Key)
import Json.Decode as Json
import OAuth exposing (Token)
import Theme exposing (Theme)
import Time exposing (Posix)
import Url exposing (Url)



--- TYPES


type alias Auth =
    { redirectUri : Url
    , error : Maybe String
    , token : Maybe Token
    , profile : Maybe Profile
    , state : String
    , instance : Server
    }


initAuth : String -> Url -> Auth
initAuth bytes url =
    { redirectUri = { url | query = Nothing, fragment = Nothing }
    , error = Nothing
    , token = Nothing
    , profile = Nothing
    , state = bytes
    , instance = "social.ttree.docker"
    }


resumeAuth : Client -> String -> Url -> Auth
resumeAuth { server, token, profile } bytes url =
    let
        a =
            initAuth bytes url
    in
    { a | instance = server, token = Just token, profile = Just profile }


type alias OAuthConfiguration =
    { authorizationEndpoint : Url
    , tokenEndpoint : Url
    , profileEndpoint : Url
    , clientId : String
    , clientSecret : String
    , scope : List String
    , profileDecoder : Json.Decoder Profile
    }


type alias Server =
    String


type alias Client =
    { server : Server
    , token : Token
    , profile : Profile
    }


type alias ProfileId =
    String


type alias Profile =
    { acct : String
    , avatar : String
    , created_at : Posix
    , display_name : String
    , followers_count : Int
    , following_count : Int
    , header : String
    , id : ProfileId
    , locked : Bool
    , note : String
    , statuses_count : Int
    , url : String
    , username : String
    }
