module Main exposing (..)

import Html
import String

upperCaseIfLong: String -> String
upperCaseIfLong name =
    if String.length name > 10 then
        String.toUpper name
    else
        name


main =
    let
        name =
            "Charlie Charlie"

        nameLength =
            String.length name
    in
        (upperCaseIfLong name)
            ++ " - name length: "
            ++ toString nameLength
            |> Html.text
