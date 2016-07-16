import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Route.QueryString exposing (..)
import WebSocket
import String

import Brisca
import Lobby
import PlayerInfo


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


briscaServer: String
briscaServer =
  "wss://brisca-server.herokuapp.com"


-- MODEL



type alias Model =
  { page: String
  , playerId: String
  , lobbyModel : Lobby.Model
  , briscaModel: Brisca.Model
  , playerInfoModel: PlayerInfo.Model
  }

init: (Model, Cmd Msg)
init =
  let
    (initLobbyModel, lobbyCmd) = Lobby.init "" ""
    (initBriscaModel, briscaCmd) = Brisca.init "Player1" "Player2"
    (initPlayerInfoModel, playerInfoCmd) = PlayerInfo.init "" -- When passing Empty String, Player Info will handle it "Gracefully"
  in
    (Model "Lobby" "" initLobbyModel initBriscaModel initPlayerInfoModel -- Player id is unknown at first Empty string
    , Cmd.batch
      [ Cmd.map LobbyMsg lobbyCmd
      , Cmd.map BriscaMsg briscaCmd
      , Cmd.map PlayerInfoMsg playerInfoCmd
      ]
    )


-- UPDATE



type Msg = ChangePage String
  | LobbyMsg Lobby.Msg
  | BriscaMsg Brisca.Msg
  | PlayerInfoMsg PlayerInfo.Msg
  | NewMessage String



update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewMessage qstr -> -- Main's webSocket subscription. Distribute all messages to their respective page to update.
      let -- Parse QueryString
        qs = parse (Debug.log "QueryString recieved in Main: " qstr)
        cmd = one string "cmd" qs |> Maybe.withDefault "Error!!!Should Never Happen"  -- handle Error
      in
        if cmd == "playerId" then -- Set Main model player Id
          let
            player = one string "player" qs |> Maybe.withDefault "Error!!!Should Never Happen"  -- handle Error
          in
          ({model | playerId = player}, Cmd.none)
        else if cmd == "updatePlayers" then  -- Pass query string to Lobby Page and re-render that page
          let
            (initLobbyModel, lobbyCmd) = Lobby.init model.playerId qstr
          in --- Check if page == "Lobby"  ???
            ( { model
              | lobbyModel = initLobbyModel
              }
            , Cmd.map LobbyMsg lobbyCmd)
        else if cmd == "playerInfo" then -- Pass query string to PlayerInfo Page and render that page
          let
            (initPlayerInfoModel, playerInfoCmd) = PlayerInfo.init qstr
          in
            ( { model
              | playerInfoModel = initPlayerInfoModel
              , page = "PlayerInfo"
              }
            , Cmd.map PlayerInfoMsg playerInfoCmd)
        else if cmd == "Error" then -- Should not happen. Throw error message to console
          let
            debugLog = (Debug.log "Error ocurred: " qstr)
          in
           (model, Cmd.none)
        else -- Future use
          (model, Cmd.none)
    ChangePage newPage -> -- For debugging only. I will Eventually delete this.
      let
        (initBriscaModel, briscaCmd) = Brisca.init model.lobbyModel.playerId "Player2"
      in
        ({model | page = newPage, briscaModel = initBriscaModel}, Cmd.map BriscaMsg briscaCmd)
    BriscaMsg subMsg ->
      let
        (updatedBriscaModel, briscaCmd) = Brisca.update subMsg model.briscaModel
      in
        ({model | briscaModel = updatedBriscaModel}, Cmd.map BriscaMsg briscaCmd)
    PlayerInfoMsg subMsg ->
      let
        (updatedPlayerInfoModel, playerInfoCmd) = PlayerInfo.update subMsg model.playerInfoModel
      in
        ({model | playerInfoModel = updatedPlayerInfoModel}, Cmd.map PlayerInfoMsg playerInfoCmd)
    LobbyMsg subMsg ->
      case subMsg of
        Lobby.PlayerClicked strPlayer -> -- Check if user clicked a player in the list
          let -- Create querystring to request player's information
            cmd = empty
            |> add "cmd" "getPlayerInfo"
            |> add "player" strPlayer
            |> render
            |> String.dropLeft 1        -- Get rid of the '?' at the beginning of qs
          in
            (model, WebSocket.send briscaServer cmd) -- Send request
--        _ ->
--          let
--            (updatedLobbyModel, lobbyCmd) = Lobby.update subMsg model.lobbyModel
--          in
--            ({model | lobbyModel = updatedLobbyModel}, Cmd.map LobbyMsg lobbyCmd)


-- SUBSCIPTIONS



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map BriscaMsg (Brisca.subscriptions model.briscaModel)
    , WebSocket.listen briscaServer NewMessage -- Belongs to Main
    ]


-- VIEW



view: Model -> Html Msg
view model =
  if model.page == "Lobby" then
    div []
      [ button [onClick (ChangePage "Brisca")][text "Press me to change page: Brisca"]
      , button [onClick (ChangePage "PlayerInfo")][text "Press me to change page: PlayerInfo"]
      , Html.map LobbyMsg (Lobby.view model.lobbyModel)
      ]
  else if model.page == "Brisca" then
    div []
      [ button [onClick (ChangePage "Lobby")][text "Press me to change page: Lobby"]
      , button [onClick (ChangePage "PlayerInfo")][text "Press me to change page: PlayerInfo"]
      , Html.map BriscaMsg (Brisca.view model.briscaModel)
      ]
  else if model.page == "PlayerInfo" then
    div []
      [ button [onClick (ChangePage "Lobby")][text "Press me to change page: Lobby"]
      , button [onClick (ChangePage "Brisca")][text "Press me to change page: Brisca"]
      , Html.map PlayerInfoMsg (PlayerInfo.view model.playerInfoModel)
      ]
  else
    div [][]
