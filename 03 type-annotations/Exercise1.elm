module Main exposing (..)
import List
import Html


type alias Item =
    { name : String, qty : Int, freeQty : Int }


cart : List Item
cart =
    [ { name = "Lemon", qty = 1, freeQty = 0 }
    , { name = "Apple", qty = 5, freeQty = 0 }
    , { name = "Pear", qty = 10, freeQty = 0 }
    ]

extraOffer: Int -> Int -> Item -> Item
extraOffer qty extra item =
    if item.qty >= qty then
        { item | freeQty =  extra }
    else
        item

newCart: List Item -> List Item
newCart =
  List.map ((extraOffer 5 1) >> (extraOffer 10 3))

main =
  newCart cart |> toString |> Html.text

