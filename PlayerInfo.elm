module PlayerInfo exposing (Model, Msg (AskToPlay,Back), init, update, view)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route.QueryString exposing (..)
import String


-- MODEL



type alias Model =
  { player: String
  , isPlaying: String
  , points: Int
  , gamesWon: Int
  , gamesLost: Int
  }


init: String -> (Model, Cmd Msg)
init qstrPlayerInfo =
  if String.isEmpty qstrPlayerInfo then
    (Model "IDK..." "false" 0 0 0, Cmd.none)
  else
    let -- Parse Query String and update Model with new values
      qs = parse qstrPlayerInfo
      player = one string "player" qs |> Maybe.withDefault "IDK"  -- handle Error
      isPlaying = one string "isPlaying" qs |> Maybe.withDefault "IDK"  -- handle Error
      points = one int "points" qs |> Maybe.withDefault 0 -- handle Error
      gamesWon = one int "gamesWon" qs |> Maybe.withDefault 0 -- handle Error
      gamesLost = one int "gamesLost" qs |> Maybe.withDefault 0 -- handle Error
    in
      (Model player isPlaying points gamesWon gamesLost, Cmd.none)


-- UPDATE



type Msg = AskToPlay String
  | Back


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    _ -> -- Handled by the Parent (Main.elm)
      (model, Cmd.none)


-- VIEW



view: Model -> Html Msg
view model =
    div []
      [ h1 [][text model.player]
      , div [][text ("Is Playing: " ++ model.isPlaying)]
      , div [][text ("Total Points: " ++ (toString model.points))]
      , div [][text ("Games Won: " ++ (toString model.gamesWon))]
      , div [][text ("Games Lost: " ++ (toString model.gamesLost))]
      , button [onClick (AskToPlay model.player)][text "Ask to Play!"]
      , button [onClick Back][text "Back"]
      ]
