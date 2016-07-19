module Waiting exposing (Model, Msg (Cancel), init, update, view)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route.QueryString exposing (..)
import String


-- MODEL



type alias Model =
  { gameId: String
  , players: List String
  }


init: String -> (Model, Cmd Msg)
init qstr =
  if String.isEmpty qstr then
    (Model "" [], Cmd.none)
  else
    let -- Parse Query String and update Model with new values
      qs = parse qstr
      gameId = one string "gameId" qs |> Maybe.withDefault "IDK"  -- handle Error
      players = all "players" qs
    in
      (Model gameId players, Cmd.none)


-- UPDATE



type Msg = Cancel


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    _ -> -- Handled by the Parent (Main.elm)
      (model, Cmd.none)


-- VIEW



view: Model -> Html Msg
view model =
    div []
      [ h1 [][text ("Game: " ++ model.gameId)]
      , viewPlayersList model
      , button [onClick Cancel][text "Cancel"]
      ]

viewPlayersList: Model -> Html Msg
viewPlayersList model =
  div []
    [ ul [] (List.map (\player -> li [][text player]) model.players)
    ]
