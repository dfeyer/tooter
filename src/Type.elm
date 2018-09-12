module Type exposing (Auth, Client, OAuthConfiguration, Account, Instance, initAuth, resumeAuth)

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
    , account : Maybe Account
    , state : String
    , instance : Instance
    }


initAuth : String -> Url -> Auth
initAuth bytes url =
    { redirectUri = { url | query = Nothing, fragment = Nothing }
    , error = Nothing
    , token = Nothing
    , account = Nothing
    , state = bytes
    , instance = "social.ttree.docker"
    }


resumeAuth : Client -> String -> Url -> Auth
resumeAuth { instance, token, account } bytes url =
    let
        a =
            initAuth bytes url
    in
    { a | instance = instance, token = Just token, account = Just account }


type alias OAuthConfiguration =
    { authorizationEndpoint : Url
    , tokenEndpoint : Url
    , accountEndpoint : Url
    , clientId : String
    , clientSecret : String
    , scope : List String
    , accountDecoder : Json.Decoder Account
    }


type alias Instance =
    String


type alias Client =
    { instance : Instance
    , token : Token
    , account : Account
    }


type alias AccountId =
    String


type alias Account =
    { acct : String
    , avatar : String
    , created_at : Posix
    , display_name : String
    , followers_count : Int
    , following_count : Int
    , header : String
    , id : AccountId
    , locked : Bool
    , note : String
    , statuses_count : Int
    , url : String
    , username : String
    }