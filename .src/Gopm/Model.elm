module Gopm.Model exposing (..)

import Time
import Browser.Navigation as Nav
import RemoteData exposing (RemoteData(..), WebData)


-- MODEL


type alias Model =
    { history : List Nav.Key
    , content : BrowserContent
    }



-- TYPE


type alias DirectoryListing =
    { metadata : CurrentFolder
    , objects : List Document
    }


type DocumentType
    = Folder
    | File


type BrowserContent
    = SingleDocument (WebData Document)
    | DocumentList (WebData DirectoryListing)
    | ContentNotAsked


type alias Station =
    { type_ : Maybe String
    , genre : Maybe String
    , name : Maybe String
    }


type alias Line =
    { type_ : Maybe String
    , owner : Maybe String
    , name : Maybe String
    }


type alias EcmMetadata =
    { name : String
    , lastUpdate : Maybe Time.Posix
    , folder : Maybe String
    , summary : Maybe String
    }


type alias GopmMetadata =
    { year : Maybe Int
    , line : Line
    , station : Station
    }


type alias Document =
    { priority : Int
    , id : String
    , key : String
    , type_ : DocumentType
    , downloadable : Bool
    , uri : Maybe String
    , name : String
    , icon : String
    , description : Maybe String
    , version : Maybe String
    , path : String
    , creationDate : Time.Posix
    , lastModificationDate : Time.Posix
    , metadata : EcmMetadata
    , gopmMetadata : GopmMetadata
    }


type alias RootlineSegment =
    { name : String
    , path : String
    , icon : String
    }


type alias CurrentFolder =
    { count : Int
    , path : String
    , rootline : List RootlineSegment
    }
