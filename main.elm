import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Lobby
import Brisca

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
  }

init: (Model, Cmd Msg)
init =
  let
    (initLobbyModel, lobbyCmd) = Lobby.init
    (initBriscaModel, briscaCmd) = Brisca.init
  in
    (Model "Lobby" initLobbyModel initBriscaModel
    , Cmd.batch
      [ Cmd.map LobbyMsg lobbyCmd
      , Cmd.map BriscaMsg briscaCmd
      ]
    )


-- UPDATE



type Msg = ChangePage String
  | LobbyMsg Lobby.Msg
  | BriscaMsg Brisca.Msg


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangePage newPage ->
      ({model | page = newPage}, Cmd.none)
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


-- SUBSCIPTIONS



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map LobbyMsg (Lobby.subscriptions model.lobbyModel)
    , Sub.map BriscaMsg (Brisca.subscriptions model.briscaModel)
    ]


-- VIEW



view: Model -> Html Msg
view model =
  if model.page == "Lobby" then
    div []
      [ button [onClick (ChangePage "Brisca")][text "Press me to change page"]
      , Html.map LobbyMsg (Lobby.view model.lobbyModel)
      ]
  else if model.page == "Brisca" then
    div []
      [ button [onClick (ChangePage "Lobby")][text "Press me to change page"]
      , Html.map BriscaMsg (Brisca.view model.briscaModel)
      ]
  else
    div [][]
