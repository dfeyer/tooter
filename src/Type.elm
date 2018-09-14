module Type exposing
    ( Account
    , AppRegistration
    , Application
    , Aside
    , Attachment
    , Auth
    , Client
    , ClientId
    , ClientSecret
    , InputInformation
    , Instance
    , Mention
    , OAuthConfiguration
    , Reblog(..)
    , Sidebar
    , Status
    , Scope
    , StatusId(..)
    , Tag
    , Timeline
    , initAuth
    , resumeAuth
    )

import Browser.Navigation as Nav exposing (Key)
import Html.Styled exposing (Html)
import Json.Decode as Json
import OAuth exposing (Token)
import RemoteData exposing (WebData)
import Json.Decode exposing (Decoder)
import Theme exposing (Theme)
import Time exposing (Posix)
import Url exposing (Protocol(..), Url)



--- TYPES

type alias Scope =
    String

type alias ClientId =
    String


type alias ClientSecret =
    String


type alias AppRegistration =
    { instance : Instance
    , scope : String
    , clientId : String
    , clientSecret : ClientSecret
    , id : String
    , redirectUri : Url
    }


type alias Sidebar msg =
    List (Html msg)


type alias Aside msg =
    List (Html msg)


type alias InputInformation =
    { status : String
    , selectionStart : Int
    }


type alias Auth =
    { error : Maybe String
    , token : Maybe Token
    , account : Maybe Account
    , state : String
    , configuration : OAuthConfiguration
    }


initAuth : Url -> OAuthConfiguration -> String -> Auth
initAuth url configuration bytes =
    { error = Nothing
    , token = Nothing
    , account = Nothing
    , state = bytes
    , configuration = configuration
    }


resumeAuth : Url -> OAuthConfiguration -> Client -> String -> Auth
resumeAuth url configuration { instance, token, account } bytes =
    let
        a =
            initAuth url configuration bytes
    in
    { a | token = Just token, account = Just account }


type alias OAuthConfiguration =
    { authorizationEndpoint : Url
    , tokenEndpoint : Url
    , accountEndpoint : Url
    , scope : List String
    , accountDecoder : Json.Decoder Account
    , redirectUri : Url
    }


type alias Timeline =
    WebData (List Status)


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
