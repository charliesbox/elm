module Main exposing (..)

import Html


add : Int -> Int -> Int
add a b =
    a + b


sumNum : List comparable -> comparable
sumNum nums =
    case nums of
        [] ->
            0

        first :: rest ->
            sumNum (rest) + first


main =
    sumNum [ 1, 2, 3 ] |> toString |> Html.text
