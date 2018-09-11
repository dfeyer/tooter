module Page.SignIn.Error exposing (toString)

import OAuth


toString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
toString { error, errorDescription } =
    let
        code =
            OAuth.errorCodeToString error

        desc =
            errorDescription
                |> Maybe.withDefault ""
                |> String.replace "+" " "
    in
    code ++ ": " ++ desc
