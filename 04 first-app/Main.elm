module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


-- Model


type alias Model =
    { calories : Int
    , input : Int
    , err : Maybe String
    }


initModel : Model
initModel =
    { calories = 0
    , input = 0
    , err = Nothing
    }



-- update


type Msg
    = AddCalorie
    | Clear
    | ChangeInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCalorie ->
            { model
                | calories = model.calories + model.input
                , input = 0
            }

        ChangeInput val ->
            case String.toInt val of
                Ok input ->
                    { model
                        | input = input
                        , err = Nothing
                    }

                Err err ->
                    { model
                        | input = 0
                        , err = Just err
                    }

        Clear ->
            initModel


validateInput : Int -> String
validateInput input =
    if input == 0 then
        ""
    else
        toString input



-- view


view : Model -> Html Msg
view model =
    div []
        [ h3 []
            [ text ("Total calories: " ++ toString model.calories) ]
        , input
            [ placeholder "Type something"
            , onInput ChangeInput
            , value (validateInput model.input)
            ]
            []
        , div [] [ text (Maybe.withDefault "" model.err) ]
        , button
            [ type_ "button"
            , onClick AddCalorie
            ]
            [ text "Add" ]
        , button
            [ type_ "button"
            , onClick Clear
            ]
            [ text "Clear" ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
