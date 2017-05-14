module Main exposing (..)

import Html
import String exposing (left)


(~=) : String -> String -> Bool
(~=) text1 text2 =
    left 1 text1 == left 1 text2


main =
    (~=) "Hello" "Li"
        |> toString
        |> Html.text
