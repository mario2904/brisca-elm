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
  , players: List String
  , gameInfo: Lobby.GameInfo
  , gameRequests: List GameRequest
  , lobbyModel : Lobby.Model
  , briscaModel: Brisca.Model
  , playerInfoModel: PlayerInfo.Model
  }


type alias GameRequest =
  { player: String  -- Player that created the game
  , gameId: String
  , numOfPlayers: String
  }


init: (Model, Cmd Msg)
init =
  let
    gameInfo = Lobby.GameInfo "" "2" [] -- The default is 2 player game
    (initLobbyModel, lobbyCmd) = Lobby.init "" gameInfo []
    (initBriscaModel, briscaCmd) = Brisca.init "Player1" "Player2"
    (initPlayerInfoModel, playerInfoCmd) = PlayerInfo.init "" -- When passing Empty String, Player Info will handle it "Gracefully"
  in
    (Model "Lobby" "" [] gameInfo [] initLobbyModel initBriscaModel initPlayerInfoModel -- Player id is unknown at first Empty string
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
  | AcceptRequest String



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
            newPlayers = all "players" qs
            (initLobbyModel, lobbyCmd) = Lobby.init model.playerId model.gameInfo newPlayers
          in --- Check if page == "Lobby"  ???
            ( { model
              | lobbyModel = initLobbyModel
              , players = newPlayers
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
        else if cmd == "requestToPlay" then
          let
            player = one string "playerFrom" qs |> Maybe.withDefault "Error!!!Should Never Happen"  -- handle Error
            gameId = one string "gameId" qs |> Maybe.withDefault "Error!!!Should Never Happen"  -- handle Error
            numOfPlayers = one string "numOfPlayers" qs |> Maybe.withDefault "Error!!!Should Never Happen"  -- handle Error
            newGameRequest = GameRequest player gameId numOfPlayers
          in
          ({model | gameRequests = newGameRequest :: model.gameRequests}, Cmd.none)
        else if cmd == "updateGameInfo" then -- Pass updated game information to GameInfo
          let
            --Update model GameInfo
            gameId = one string "gameId" qs |> Maybe.withDefault "IDK"  -- handle Error
            numOfPlayers = one string "numOfPlayers" qs |> Maybe.withDefault "IDK"  -- handle Error
            players = all "players" qs
            newGameInfo = Lobby.GameInfo gameId numOfPlayers players
            (initLobbyModel, lobbyCmd) = Lobby.init model.playerId newGameInfo model.players
            --Update Lobby GameInfo
          in
            ( { model
              | lobbyModel = initLobbyModel
              , gameInfo = newGameInfo
              }
            , Cmd.map LobbyMsg lobbyCmd)
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
      case subMsg of
        PlayerInfo.AskToPlay strPlayer -> -- Send ask request and return to Lobby Page
          let -- Create querystring to ask a player to play a game
            cmd = empty
            |> add "cmd" "askToPlay"
            |> add "playerTo" strPlayer
            |> render
            |> String.dropLeft 1        -- Get rid of the '?' at the beginning of qs
          in
            ({model | page = "Lobby"}, WebSocket.send briscaServer cmd)
        PlayerInfo.Back ->
          ({model | page = "Lobby"}, Cmd.none)
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
        Lobby.CreateGame -> -- Create a new game
          let -- Create querystring to Create a Game
            cmd = empty
            |> add "cmd" "createGame"
            |> add "numOfPlayers" model.gameInfo.numOfPlayers
            |> render
            |> String.dropLeft 1        -- Get rid of the '?' at the beginning of qs
          in
            (model, WebSocket.send briscaServer cmd) -- Send request
        Lobby.Select num -> -- Update GameInfo numOfPlayers
          let -- OverWrite Everything. Doesn't matter, game isn't created yet
            newGameInfo = Lobby.GameInfo "" num []
          in
            ({model | gameInfo = newGameInfo}, Cmd.none)
    AcceptRequest gameId -> -- Accept game request
      let -- Create querystring to send acceptance of game request
        cmd = empty
        |> add "cmd" "acceptRequestToPlay"
        |> add "gameId" gameId
        |> render
        |> String.dropLeft 1        -- Get rid of the '?' at the beginning of qs
      in
        (model, WebSocket.send briscaServer cmd)

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
      , Html.map LobbyMsg (Lobby.view model.lobbyModel)
      , viewGameRequests model
      ]
  else if model.page == "Brisca" then
    div []
      [ button [onClick (ChangePage "Lobby")][text "Press me to change page: Lobby"]
      , Html.map BriscaMsg (Brisca.view model.briscaModel)
      ]
  else if model.page == "PlayerInfo" then
    div []
      [ Html.map PlayerInfoMsg (PlayerInfo.view model.playerInfoModel)
      ]
  else
    div [][]


-- VIEW GAMEREQUESTS


viewGameRequests: Model -> Html Msg
viewGameRequests model =
  div []
    [ h1 [][text "Game Requests:"]
    , ul []
      (List.map (\req ->
        li []
          [ div [] [text ("GameID: " ++ req.gameId)]
          , div [] [text ("Number of Players: " ++ req.numOfPlayers)]
          , div [] [text ("Creator: " ++ req.player)]
          , button [onClick (AcceptRequest req.gameId)][text "Accept"]
          ]
      ) model.gameRequests)
    ]
