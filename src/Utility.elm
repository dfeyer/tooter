module Utility exposing (toJson)

import Json.Encode as Encode


toJson : Encode.Value -> String
toJson =
    Encode.encode 0
