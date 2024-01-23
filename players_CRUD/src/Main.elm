module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)


initPlayer : Int -> Player
initPlayer id =
    Player id "" False


type alias Player =
    { id : Int
    , name : String
    , isActive : Bool
    }


type alias Model =
    { players : List Player
    , newPlayer : Player
    }


type Msg
    = SetName String
    | AddPlayer
    | ModifyPlayer Int Bool
    | DeletePlayer Int


init : Model
init =
    { players = []
    , newPlayer = initPlayer 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetName name ->
            let
                updatedNewPlayer = { name = name, id = model.newPlayer.id, isActive = model.newPlayer.isActive }
            in
            { model | newPlayer = updatedNewPlayer }

        AddPlayer ->
            let
                newId =
                    if List.isEmpty model.players then
                        0
                    else
                        List.length model.players
                newPlayer = { id = newId, name = model.newPlayer.name, isActive = False }
            in
            { model | players = model.players ++ [newPlayer], newPlayer = initPlayer (newId + 1) }
        DeletePlayer id ->
            { model | players = List.filter (\player -> player.id /= id) model.players }

        ModifyPlayer id status ->
            let
                updatePlayer player =
                    if player.id == id then
                        { player | isActive = not player.isActive }
                    else
                        player
            in
            { model | players = List.map updatePlayer model.players }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm Exercise: Players CRUD" ]
        , Html.form [ id "submit-player", onSubmit AddPlayer ]
            [ input [ id "input-player", type_ "text", value model.newPlayer.name, onInput SetName ] []
            , button [ id "btn-add", type_ "submit" ] [ text "Add Player" ]
            ]
        , ol [ id "players-list" ]
            (List.map viewPlayer model.players)
        ]

viewPlayer : Player -> Html Msg
viewPlayer player =
    let
        statusText =
            if player.isActive then
                "Active"
            else
                "Inactive"
    in
    li [ id <| "player-" ++ String.fromInt player.id ]
        [ div [ class "player-name" ] [ text player.name ]
        , label [ class "player-status" ]
            [ input
                [ type_ "checkbox"
                , checked player.isActive
                , onClick (ModifyPlayer player.id (not player.isActive))
                , class "player-status"
                ] []
            , text statusText -- Display "Active" or "Inactive" based on player's isActive status
            ]
        , button [ class "btn-delete", onClick (DeletePlayer player.id) ] [ text "Delete" ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
