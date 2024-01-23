-- Fetch players from end point on load
-- Update the id from the fetched players
-- Add player to the end of the list


module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, field, map3)


type alias Player =
    { id : Int
    , name : String
    , isActive : Bool
    }


type alias Model =
    { players : List Player
    , newPlayer : Player
    , reqStatus : String
    }


type Msg
    = SetName String
    | ModifyPlayer Int Bool
    | AddPlayer
    | DeletePlayer Int
    | FetchPlayers (Result Http.Error (List Player))


playerDecoder : Decoder Player
playerDecoder =
    map3 Player (field "id" Decode.int) (field "name" Decode.string) (field "isActive" Decode.bool)


playersDecoder : Decoder (List Player)
playersDecoder =
    Decode.list playerDecoder


fetchPlayers : String -> Cmd Msg
fetchPlayers url = 
    Http.get
             { url = url
             , expect = Http.expectJson FetchPlayers playersDecoder
             }



listLast : List a -> Maybe a
listLast list =
    List.head <| List.reverse list


initPlayer : Int -> Player
initPlayer id =
    Player id "" False


init : () -> ( Model, Cmd Msg )
init _ =
    ( { 
        players = []
      , newPlayer = initPlayer 0
      , reqStatus = "Loading..."
    }
    , fetchPlayers "http://localhost:3001/api/players/"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName word ->
            let
                updatedNewPlayer = { name= word, id = model.newPlayer.id, isActive = model.newPlayer.isActive}
            in
            ({model | newPlayer = updatedNewPlayer}, Cmd.none)    

        AddPlayer ->
            let
                newId = 
                   if List.isEmpty model.players then
                       0
                   else
                       List.length model.players + 1
                newPlayer = { id = newId, name = model.newPlayer.name, isActive = False }
            in
            ( { model | players = model.players ++ [newPlayer], newPlayer = initPlayer (newId + 2) }, Cmd.none )

        DeletePlayer id ->
            ( { model | players = List.filter (\player -> player.id /= id) model.players }, Cmd.none )

        ModifyPlayer id status ->
            let
                updatePlayer player =
                    if player.id == id then
                        { player | isActive = not player.isActive}
                    else
                        player
            in                
            ( {model | players = List.map updatePlayer model.players }, Cmd.none )

        FetchPlayers result ->
            case result of
                Ok fetchedPlayers ->
                    ({ model | players = fetchedPlayers , reqStatus = ""}, Cmd.none)

                Err _ ->
                    ({ model | reqStatus = " an error has occurred!!!"  }, Cmd.none)    
            


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm Exercise: Players Fetch" ]
        , Html.form [ id "submit-player", onSubmit AddPlayer]
              [ input [id "input-player", type_ "text", value model.newPlayer.name, onInput SetName ] []
              , button [ id "btn-add", type_ "submit"] [ text "Add Player"]
              ]
        , ol [ id "players-list" ]
            (List.map viewPlayer model.players)
        , div [ id "request-status" ] [ text model.reqStatus]
        ]          

viewPlayer : Player -> Html Msg
viewPlayer player = 
    let
        statusText = 
            if player.isActive then
                "Active"
            else
                "Not Active"   
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
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
