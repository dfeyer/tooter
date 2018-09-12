port module Ports exposing (deleteRegistration, saveClients, saveRegistration)


port saveRegistration : String -> Cmd msg


port deleteRegistration : String -> Cmd msg


port saveClients : String -> Cmd msg
