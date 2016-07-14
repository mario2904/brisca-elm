module PlayerInfo exposing ( Model, Msg, init, update, view, subscriptions)

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
  { player: String
  , isPlaying: String
  , points: Int
  , gamesWon: Int
  , gamesLost: Int
  }


init: String -> (Model, Cmd Msg)
init strPlayer =
  let
    -- Create querystring to fetch player info
    cmd = empty
    |> add "cmd" "getPlayerInfo"
    |> add "player" strPlayer
    |> render
  in
    (Model strPlayer "False" 0 0 0, WebSocket.send briscaServer cmd)


-- UPDATE



type Msg = NewMessage String


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewMessage str ->
      let
        qs = parse str
        cmd = one string "cmd" qs |> Maybe.withDefault ""  -- handle Error
      in
        if cmd == "playerInfo" then
          let
            isPlaying = one string "isPlaying" qs |> Maybe.withDefault "False"  -- handle Error
            points = one int "points" qs |> Maybe.withDefault 0 -- handle Error
            gamesWon = one int "gamesWon" qs |> Maybe.withDefault 0 -- handle Error
            gamesLost = one int "gamesLost" qs |> Maybe.withDefault 0 -- handle Error
          in
            (Model model.player isPlaying points gamesWon gamesLost, Cmd.none)
        else
          ({model | player = "Error"}, Cmd.none) -- Send Error


-- SUBSCIPTIONS



subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen briscaServer NewMessage


-- VIEW



view: Model -> Html Msg
view model =
    div []
      [ h1 [][text model.player]
      , div [][text ("Is Playing: " ++ model.isPlaying)]
      , div [][text ("Total Points: " ++ (toString model.points))]
      , div [][text ("Games Won: " ++ (toString model.gamesWon))]
      , div [][text ("Games Lost: " ++ (toString model.gamesLost))]
      ]
