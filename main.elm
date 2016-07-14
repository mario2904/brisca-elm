import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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


-- MODEL



type alias Model =
  { page: String
  , lobbyModel : Lobby.Model
  , briscaModel: Brisca.Model
  , playerInfoModel: PlayerInfo.Model
  }

init: (Model, Cmd Msg)
init =
  let
    (initLobbyModel, lobbyCmd) = Lobby.init
    (initBriscaModel, briscaCmd) = Brisca.init "Player1" "Player2"
    (initPlayerInfoModel, playerInfoCmd) = PlayerInfo.init "Loading..."
  in
    (Model "Lobby" initLobbyModel initBriscaModel initPlayerInfoModel
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


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangePage newPage ->
      let
        (initBriscaModel, briscaCmd) = Brisca.init model.lobbyModel.playerId "Player2"
      in
        ({model | page = newPage, briscaModel = initBriscaModel}, Cmd.map BriscaMsg briscaCmd)
    LobbyMsg subMsg ->
      let
        (updatedLobbyModel, lobbyCmd) = Lobby.update subMsg model.lobbyModel
      in
        ({model | lobbyModel = updatedLobbyModel}, Cmd.map LobbyMsg lobbyCmd)
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


-- SUBSCIPTIONS



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map LobbyMsg (Lobby.subscriptions model.lobbyModel)
    , Sub.map BriscaMsg (Brisca.subscriptions model.briscaModel)
    , Sub.map PlayerInfoMsg (PlayerInfo.subscriptions model.playerInfoModel)
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
