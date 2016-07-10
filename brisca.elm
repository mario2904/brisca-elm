import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL



type alias Model =
  { players: List Player
  , board: String
  }

type alias Player =
  { cards: List Card
  , playerId: String
  }

type alias Card =
  { name: String
  , cardMsg: Msg
  , x: Int
  , y: Int
  }

init: (Model, Cmd Msg)          -- For testing purposes... This will
init =                          -- definitely change later on
  let                           -- in the mean time hardcode the initial state
    cardImg = "img/flippedVertical.jpg"

    p1_Y = c.boardHeight - c.cardHeight - (2 * c.margin)

    p_X1 = (c.boardWidth // 2) - ((c.cardWidth * 3) // 2) - (2 * c.margin)
    p_X2 = p_X1 + c.cardWidth + (2 * c.margin)
    p_X3 = p_X2 + c.cardWidth + (2 * c.margin)

    cardP1_1 = Card cardImg Card1 p_X1 p1_Y
    cardP1_2 = Card cardImg Card2 p_X2 p1_Y
    cardP1_3 = Card cardImg Card3 p_X3 p1_Y

    cardP2_1 = Card cardImg CardX p_X1 0
    cardP2_2 = Card cardImg CardX p_X2 0
    cardP2_3 = Card cardImg CardX p_X3 0

    player1 = Player [cardP1_1, cardP1_2, cardP1_3] "Player1"
    player2 = Player [cardP2_1, cardP2_2, cardP2_3] "Player2"
    players = [player1, player2]

    board = "img/board.jpg"

  in
    (Model players board, Cmd.none)


-- UPDATE



type Msg = CardX 
  | Card1
  | Card2
  | Card3                  -- User clicked on other player's card
                            -- Future use... Maybe show a message or something


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    _ ->
      (model, Cmd.none)



subscriptions : Model -> Sub Msg            -- For later use in animation
subscriptions model =
  Sub.none


-- VIEW



view: Model -> Html Msg
view model =
  div [divStyle] (viewCards model)


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


-- VIEW FUNCTION HELPERS



viewCards: Model -> List (Html Msg)
viewCards model =
  List.concatMap (\player ->             -- Test here for player.playerId
    List.map (\{name, cardMsg, x, y} ->
      img [src name, (imgStyle x y), onClick cardMsg] []) player.cards) model.players
