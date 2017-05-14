module Main exposing (..)

import Html
import List exposing (length)
import String exposing (words)

wordCount: String -> Int
wordCount =
  words >> length


main =
    wordCount "Hey hello world"
    |> toString
    |> Html.text
