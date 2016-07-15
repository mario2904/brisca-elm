module Lobby exposing ( Model, Msg (PlayerClicked), init, update, view, subscriptions )

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route.QueryString exposing (..)
import WebSocket


briscaServer: String
briscaServer =
  "wss://brisca-server.herokuapp.com"


-- MODEL



type alias Model =
  { players: List String
  , playerId: String
  , playerClicked: String
  }


init: (Model, Cmd Msg)
init =
  (Model [] "" "", Cmd.none)


-- UPDATE



type Msg = NewMessage String
  | PlayerClicked String


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewMessage str ->   -- PARSE QUERYSTRING AND RESOLVE
      let
        qs = parse str
        cmd = one string "cmd" qs |> Maybe.withDefault ""  -- handle Error
      in
        if cmd == "updatePlayers" then                  -- Update List Players
          let
            newPlayers = all "players" qs
          in
            ({model | players = newPlayers}, Cmd.none)
        else if cmd == "playerId" then                  -- Update my PlayerId
          let
            newPlayerId = one string "player" qs |> Maybe.withDefault "Guest"-- handle Error
          in
            ({model | playerId = newPlayerId}, Cmd.none)
        else
          (model, Cmd.none)
    PlayerClicked str ->
      ({model | playerClicked = (Debug.log "Player Clicked: "str)}, Cmd.none)


-- SUBSCRIPTIONS



subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen briscaServer NewMessage


-- VIEW



view: Model -> Html Msg
view model =
  div[]
    [ h1 [] [text model.playerId]
    , div [] (viewList model)
    ]


-- STYLES



divStyle: Attribute msg
divStyle =
  style
    [ ("margin-top", "5px")
    , ("padding", "10px")
    , ("border", "1px solid #ddd")
    , ("border-radius", "4px")
    , ("cursor", "pointer")
--    , ("max-width", "150px")
    ]


-- VIEW PLAYERS LIST



viewList: Model -> List (Html Msg)
viewList model =
  List.map (\player -> div [divStyle, onClick (PlayerClicked player)][text player]) model.players
