module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Dict exposing (Dict)


-- model


type alias Players =
    Dict Int Player


type alias Plays =
    Dict Int Play


type alias Model =
    { players : Players
    , name : String
    , playerId : Maybe Int
    , plays : Plays
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , points : Int
    }


initModel : Model
initModel =
    { players = Dict.empty
    , name = ""
    , playerId = Nothing
    , plays = Dict.empty
    }



-- update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


addPlayer : Model -> Model
addPlayer model =
    let
        numberOfPlayers =
            model.players |> Dict.size
    in
        -- using cons (::) cause is more performant than append
        -- newPlayer :: model.players
        { model
            | players = dictInsertUpdate model numberOfPlayers
            , name = ""
            , playerId = Nothing
        }


editPlayer : Model -> Int -> Model
editPlayer model id =
    { model
        | players = dictInsertUpdate model id
        , name = ""
        , playerId = Nothing
    }


score : Model -> Player -> Int -> Model
score model player points =
    let
        playId =
            model.plays |> Dict.size

        newPlays =
            Dict.insert playId (Play playId player.id points) model.plays
    in
        { model | plays = newPlays }


deletePlay : Model -> Play -> Model
deletePlay model play =
    let
        newPlays =
            Dict.remove play.id model.plays
    in
        { model | plays = newPlays }


getPlayerPoints : Int -> Plays -> Int
getPlayerPoints playerId plays =
    let
        getPoints playerId play =
            if play.playerId == playerId then
                Just play.points
            else
                Nothing
    in
        plays
            |> Dict.values
            |> List.filterMap (getPoints playerId)
            |> List.sum


dictInsertUpdate : Model -> Int -> Players
dictInsertUpdate model id =
    case (Dict.get id model.players) of
        Nothing ->
            let
                newPlayer =
                    Player id model.name 0
            in
                Dict.insert id newPlayer model.players

        Just currentPlayer ->
            updateDict id (\player -> { player | name = model.name }) model.players


updateDict : comparable -> (b -> b) -> Dict comparable b -> Dict comparable b
updateDict key map dict =
    Dict.update key (Maybe.map map) dict


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input name ->
            Debug.log "Input Updated "
                { model | name = name }

        Save ->
            case model.playerId of
                Just playerId ->
                    editPlayer model playerId

                Nothing ->
                    addPlayer model

        Cancel ->
            { model | name = "", playerId = Nothing }

        Edit player ->
            { model | name = player.name, playerId = Just player.id }

        Score player points ->
            score model player points

        DeletePlay player ->
            deletePlay model player



-- view


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playerSection model
        , playerForm model
        , playSection model
        ]


playSection : Model -> Html Msg
playSection model =
    div []
        [ playListHeader
        , playList model
        ]


playListHeader : Html Msg
playListHeader =
    header []
        [ div []
            [ text "Plays" ]
        , div
            []
            [ text "Points" ]
        ]


playList : Model -> Html Msg
playList model =
    model.plays
        |> Dict.values
        |> List.map (playRow model.players)
        |> List.reverse
        |> ul []


playRow : Players -> Play -> Html Msg
playRow players play =
    let
        player =
            Dict.get play.playerId players
                |> Maybe.map (.name)
    in
        li []
            [ i
                [ class "remove"
                , onClick (DeletePlay play)
                ]
                []
            , div [] [ text (Maybe.withDefault "unknown" player) ]
            , div [] [ text (toString play.points) ]
            ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "text"
            , placeholder "Add or Edit"
            , onInput Input
            , value model.name
            ]
            []
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick Cancel ] [ text "Cancel" ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerListHeader
        , playerList model
        , pointTotal model
        ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerRow : Plays -> Player -> Html Msg
playerRow plays player =
    li []
        [ i
            [ class "edit"
            , onClick (Edit player)
            ]
            []
        , div [] [ text player.name ]
        , button
            [ type_ "button"
            , onClick (Score player 2)
            ]
            [ text "2 pts" ]
        , button
            [ type_ "button"
            , onClick (Score player 3)
            ]
            [ text "3 pts" ]
        , div [] [ text (toString (getPlayerPoints player.id plays)) ]
        ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        total =
            model.plays
                |> Dict.values
                |> List.map .points
                |> List.sum
    in
        footer []
            [ div [] [ text "Total: " ]
            , div [] [ text (toString total) ]
            ]


playerList : Model -> Html Msg
playerList model =
    model.players
        |> Dict.values
        |> List.sortBy .name
        |> List.map (playerRow model.plays)
        |> ul []


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
