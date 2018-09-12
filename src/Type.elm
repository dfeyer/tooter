module Type exposing (Auth, OAuthConfiguration, Profile, initAuth)

import Browser.Navigation as Nav exposing (Key)
import Json.Decode as Json
import OAuth
import Theme exposing (Theme)
import Time exposing (Posix)
import Url exposing (Url)



--- TYPES


type alias Auth =
    { redirectUri : Url
    , error : Maybe String
    , token : Maybe OAuth.Token
    , profile : Maybe Profile
    , state : String
    , instance : String
    }


initAuth : String -> Url -> Auth
initAuth bytes origin =
    { redirectUri = { origin | query = Nothing, fragment = Nothing }
    , error = Nothing
    , token = Nothing
    , profile = Nothing
    , state = bytes
    , instance = "social.ttree.docker"
    }


type alias OAuthConfiguration =
    { authorizationEndpoint : Url
    , tokenEndpoint : Url
    , profileEndpoint : Url
    , clientId : String
    , clientSecret : String
    , scope : List String
    , profileDecoder : Json.Decoder Profile
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
