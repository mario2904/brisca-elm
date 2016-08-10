module Lobby exposing (Model, GameInfo, Msg (PlayerClicked, CreateGame, Select), init, update, view)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route.QueryString exposing (..)
import Json.Decode
import WebSocket
import String


-- MODEL



type alias Model =
  { playerId: String
  , gameInfo: GameInfo
  , players: List String
  }


type alias GameInfo =
  { gameId: String
  , numOfPlayers: String
  , players: List String
  }


init: String -> GameInfo -> (List String) -> (Model, Cmd Msg)
init player gameInfo players =
      (Model player gameInfo players, Cmd.none)


-- UPDATE



type Msg = PlayerClicked String
  | CreateGame
  | Select String


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PlayerClicked str ->  -- Handled by the Parent (Main.elm)
      (model, Cmd.none)
    CreateGame ->         -- Handled by the Parent (Main.elm)
      (model, Cmd.none)
    Select num ->         -- Handled by the Parent (Main.elm)
      (model, Cmd.none)


-- VIEW



view: Model -> Html Msg
view model =
  div[]
    [ h1 [] [text ("User ID: " ++ model.playerId)]
    , h1 [] [text "Game info: "]
    , viewGameInfo model
    , h1 [] [text "Online Users: "]
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


-- VIEW GAME INFO



viewGameInfo: Model -> Html Msg
viewGameInfo model =
  if String.isEmpty model.gameInfo.gameId then
    div [style [("border", "1px solid #ddd")]]
      [ h3 [][ text "Not registered in an existing game. "]
      , div []
        [ text "Number of Players: "
        , select [on "change" (Json.Decode.map Select targetValue)]
          [ option [value "2"] [text "2"]
          , option [value "4"] [text "4"]
          ]
        ]
      , button [onClick CreateGame][text "Create Game"]
      ]
  else
    div[style [("border", "1px solid #ddd")]]
    [ div [] [text ("Game ID: " ++ model.gameInfo.gameId)]
    , div [] [text ("Number of players: " ++ model.gameInfo.numOfPlayers)]
    , ul [] (List.map (\player -> li [][text player]) model.gameInfo.players)
    ]


-- VIEW PLAYERS LIST



viewList: Model -> List (Html Msg)
viewList model =
  List.map (\player -> div [divStyle, onClick (PlayerClicked player)][text player]) model.players
