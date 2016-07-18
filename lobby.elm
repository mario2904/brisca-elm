module Lobby exposing (Model, Msg (PlayerClicked), init, update, view)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route.QueryString exposing (..)
import WebSocket
import String


-- MODEL



type alias Model =
  { playerId: String
  , players: List String
  , playerClicked: String
  }


init: String -> String -> (Model, Cmd Msg)
init player qstr =
  if (String.isEmpty player) || (String.isEmpty qstr) then
    (Model "" [] "", Cmd.none)
  else
    let
      qs = parse qstr
      newPlayers = all "players" qs
    in
      (Model player newPlayers "", Cmd.none)


-- UPDATE



type Msg = PlayerClicked String


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PlayerClicked str -> -- Handled by the Parent (Main.elm)
      ({model | playerClicked = (Debug.log "Player Clicked: "str)}, Cmd.none)


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
