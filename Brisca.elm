module Brisca exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket


-- CONSTANTS
type alias Constants =
  { boardWidth: Int
  , boardHeight: Int
  , cardWidth: Int
  , cardHeight: Int
  , margin: Int
  , step: Int
  }


c: Constants
c =
  { boardWidth = 700
  , boardHeight = 700
  , cardWidth = 100
  , cardHeight = 139
  , margin = 5
  , step = 5
  }


briscaServer: String
briscaServer =
  "wss://brisca-server.herokuapp.com"


-- MODEL



type alias Model =
  { players: List Player
  , board: String
  , input : String          -- For use in Testing the brisca-server
  , response: String        -- For use in Testing the brisca-server
  , status: String
  }

type alias Player =
  { cards: List Card
  , playerId: String
  , played: Bool
  }

type alias Card =
  { name: String
  , cardPos: Int            -- [0,1,2]
  , x: Int
  , y: Int
  }

init: String -> (List String) -> String -> (List String) -> (Model, Cmd Msg)
init strPlayer1 strPlayers life cards =
  let                           -- in the mean time hardcode the initial state for 2 player game
    cardImg = "img/flippedVertical.jpg"
    -- Extract the 3 cards
    card1 = List.head cards |> Maybe.withDefault "Error"
    card2 = List.tail cards |> Maybe.withDefault [] |> List.head |> Maybe.withDefault "Error"
    card3 = List.tail cards |> Maybe.withDefault [] |> List.tail |> Maybe.withDefault [] |> List.head |> Maybe.withDefault "Error"
    --  Re-order players (First is "Me")
    orderedPlayers = rearrangeOrder strPlayer1 strPlayers
    orderedPlayer1 = List.head orderedPlayers |> Maybe.withDefault "Error"
    orderedPlayer2 = List.tail orderedPlayers |> Maybe.withDefault [] |> List.head |> Maybe.withDefault "Error"

    p1_Y = c.boardHeight - c.cardHeight - (2 * c.margin)

    p_X1 = (c.boardWidth // 2) - ((c.cardWidth * 3) // 2) - (2 * c.margin)
    p_X2 = p_X1 + c.cardWidth + (2 * c.margin)
    p_X3 = p_X2 + c.cardWidth + (2 * c.margin)

    cardP1_1 = Card ("img/" ++ card1 ++ ".jpg") 0 p_X1 p1_Y
    cardP1_2 = Card ("img/" ++ card2 ++ ".jpg") 1 p_X2 p1_Y
    cardP1_3 = Card ("img/" ++ card3 ++ ".jpg") 2 p_X3 p1_Y

    cardP2_1 = Card cardImg 0 p_X1 0
    cardP2_2 = Card cardImg 1 p_X2 0
    cardP2_3 = Card cardImg 2 p_X3 0

    player1 = Player [cardP1_1, cardP1_2, cardP1_3] orderedPlayer1 False
    player2 = Player [cardP2_1, cardP2_2, cardP2_3] orderedPlayer2 False
    players = [player1, player2]

    board = "img/board.jpg"

  in
    (Model players board "" "" "Game Started", Cmd.none)


-- UPDATE



type Msg = Move String Int
  | Input String
  | Send
  | NewMessage String


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Move id cardPos ->
      case (List.head model.players) of       -- Player (himself) is always
        Nothing ->                            -- the head of the players list
          (model, Cmd.none)
        Just player ->
          if player.playerId == id then
            moveCard up id cardPos model
          else
            ({model | status = "Don't touch other player cards!"}, Cmd.none)

    Input newInput->
      ({model | input = newInput}, Cmd.none)
    Send ->
      (model, WebSocket.send briscaServer model.input)
    NewMessage str ->
      ({model | response = str}, Cmd.none)


-- SUBSCRIPTIONS



subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen briscaServer NewMessage


-- VIEW



view: Model -> Html Msg
view model =
  div[]
    [ div [divStyle] (viewCards model)
    , h1 [][text model.status]
    , (viewPlayers model)
    , div []
      [ input [onInput Input] []
      , button [onClick Send] [text "Send"]
      , div [] [text model.response]
      ]
    ]


-- STYLES



divStyle: Attribute msg
divStyle =
  style
    [ ("background-image", "url(img/board.jpg)")
    , ("position", "relative")
    , ("margin","0 auto")
    , ("width", (toString c.boardWidth) ++ "px")
    , ("height", (toString c.boardHeight) ++ "px")
--    , ("overflow", "hidden")
    ]


imgStyle : Int -> Int -> Attribute msg
imgStyle x y =
  style
    [ ("position", "absolute")
    , ("left", (toString x) ++ "px")
    , ("top", (toString y) ++ "px")
    , ("margin", (toString c.margin) ++ "px")
    ]


-- TRANSFORM CARD FUNCTIONS



up: Card -> Card
up card =
  {card | y = (card.y - c.cardHeight - c.margin)} -- Move up


down: Card -> Card
down card =
  {card | y = (card.y + c.cardHeight + c.margin)} -- Move up


-- VIEW FUNCTION HELPERS



moveCard: (Card -> Card) -> String -> Int -> Model -> (Model, Cmd Msg)
moveCard cardTransform id cardPos model =
  case (List.head (List.filter (\player -> player.playerId == id) model.players)) of
    Nothing ->
      (model,Cmd.none)
    Just player ->
      if player.played then
        ({model | status = "You already played this round."},Cmd.none)
      else
        case (List.head (List.filter (\card -> card.cardPos == cardPos) player.cards)) of
          Nothing ->
            (model,Cmd.none)
          Just card ->
            let -- Update card
              newCard = cardTransform card -- moveCard
              newCards = (newCard :: (List.filter (\tmp -> tmp /= card) player.cards))
              newPlayer = {player | cards = newCards, played = True}  -- Assert that player has played his card
              newPlayers = newPlayer :: (List.filter (\tmp -> tmp /= player) model.players)
              newModel = {model | players = newPlayers}
            in
              (newModel, Cmd.none )


viewCards: Model -> List (Html Msg)
viewCards model =
  List.concatMap (\player ->
    List.map (\{name, cardPos, x, y} ->
      img [src name, (imgStyle x y), onClick (Move player.playerId cardPos)] []) player.cards) model.players


viewPlayers: Model -> Html Msg
viewPlayers model =
  div []
    [ h1 [][text "Players:"]
    , ol [] (List.map (\player -> li [][text player.playerId]) model.players)
    ]


-- PRIVATE HELPER FUNCTIONS



rearrangeOrder: String -> (List String) -> (List String)
rearrangeOrder player players =
  case List.head players of
    Nothing -> []
    Just head ->
      if head == player then
        players
      else
        List.append (List.drop 1 players) [head]
