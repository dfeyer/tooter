module Type exposing
    ( Account
    , Application
    , Attachment
    , Auth
    , Client
    , InputInformation
    , Instance
    , Mention
    , OAuthConfiguration
    , Reblog(..)
    , Status
    , StatusId(..)
    , Tag
    , initAuth
    , resumeAuth
    )

import Browser.Navigation as Nav exposing (Key)
import Json.Decode as Json
import OAuth exposing (Token)
import Theme exposing (Theme)
import Time exposing (Posix)
import Url exposing (Url)



--- TYPES


type alias InputInformation =
    { status : String
    , selectionStart : Int
    }


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


type alias Application =
    { name : String
    , website : Maybe String
    }


type StatusId
    = StatusId String


type alias Status =
    { account : Account
    , application : Maybe Application
    , content : String
    , created_at : String
    , favourited : Maybe Bool
    , favourites_count : Int
    , id : StatusId
    , in_reply_to_account_id : Maybe String
    , in_reply_to_id : Maybe StatusId
    , media_attachments : List Attachment
    , mentions : List Mention
    , reblog : Maybe Reblog
    , reblogged : Maybe Bool
    , reblogs_count : Int
    , sensitive : Maybe Bool
    , spoiler_text : String
    , tags : List Tag
    , uri : String
    , url : Maybe String
    , visibility : String
    }


type alias Mention =
    { id : AccountId
    , url : String
    , username : String
    , acct : String
    }


type alias Attachment =
    -- type_: -- "image", "video", "gifv"
    { id : String
    , type_ : String
    , url : String
    , remote_url : String
    , preview_url : String
    , text_url : Maybe String
    }


type Reblog
    = Reblog Status


type alias Tag =
    { name : String
    , url : String
    }
