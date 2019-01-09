import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Tuple
import Svg 
import Svg.Attributes exposing (..)
import List.Extra exposing (..)
import Time


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  { liveCells : List (Int, Int)
  , timeStep : Int
  , paused : Bool
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model [(2+10,20),(3+10,20),(4+10,20),(5+10,20),(6+10,20),(7+10,20),(8+10,20),(9+10,20),(11+10,20),(12+10,20),(13+10,20),(14+10,20),(15+10,20),(19+10,20),(20+10,20),(21+10,20),(28+10,20),(29+10,20),(30+10,20),(31+10,20),(32+10,20),(33+10,20),(34+10,20),(36+10,20),(37+10,20),(38+10,20),(39+10,20),(40+10,20)] 0 False
--   , Random.generate NewState <| randomCells 800
  , Cmd.none
  )


 -- TODO: get rid of these global vars 
gridWidth = 75
gridHeight = 50
cellSize = 10 -- Note: cell's internal size will be (cellSize-1)*(cellSize-1) 



-- UPDATE


type Msg
  = Step Time.Posix
  | NewState (List (Int, Int))
  | Pause Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Step newTime ->
      if model.paused then
        (model, Cmd.none)
      else 
        ( { model | liveCells = 
            (List.foldr (++) [] <| List.map (gameOfLife model.liveCells) model.liveCells)
            ++ (List.foldr (++) [] <| List.map (gameOfDeath model.liveCells) <| cartesian (List.range 0 (gridWidth-1)) (List.range 0 (gridHeight-1)))
            , timeStep = (model.timeStep + 1)}
        , Cmd.none
        )

    NewState newLiveCells ->
      ( { model | liveCells = newLiveCells }
      , Cmd.none
      )

    Pause newState ->
      ( { model | paused = newState }
      , Cmd.none
      )


randomCell : Random.Generator (Int, Int)
randomCell = Random.pair (Random.int 0 (gridWidth-1)) (Random.int 0 (gridHeight-1))

randomCells : Int -> Random.Generator (List (Int, Int))
randomCells n = Random.list n randomCell

cartesian : List a -> List b -> List (a,b)
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs

liveneighbors : List (Int, Int) -> (Int, Int) -> List (Int, Int)
liveneighbors state (x, y) = 
  cartesian (List.range (x-1) (x+1)) (List.range (y-1) (y+1))
  |> List.filter (\a -> List.member a state)

rule1 : Int -> (Int, Int) -> Maybe (Int, Int)
rule1 neighborCount cell =
  if neighborCount < 2 then Nothing else Just cell

rule2 : Int -> (Int, Int) -> Maybe (Int, Int)
rule2 neighborCount cell =
  if neighborCount == 2 || neighborCount == 3 then Just cell else Nothing

rule3 : Int -> (Int, Int) -> Maybe (Int, Int)
rule3 neighborCount cell =
  if neighborCount > 3 then Nothing else Just cell

rule4 : Int -> (Int, Int) -> List (Int, Int)
rule4 neighborCount cell =
  if neighborCount == 3 then [cell] else []

gameOfLife : List (Int, Int) -> (Int, Int) -> List (Int, Int)
gameOfLife liveCells cell =
  let
    neighbors = liveneighbors liveCells cell
    neighborCount = (List.length neighbors) - 1
  in
    case rule1 neighborCount cell of
      Nothing -> []
      Just x -> 
        case rule2 neighborCount cell of
          Nothing -> []
          Just y ->
            case rule3 neighborCount cell of
                Nothing -> []
                Just z -> [z]

gameOfDeath : List (Int, Int) -> (Int, Int) -> List (Int, Int)
gameOfDeath liveCells cell =
    let
        neighbors = liveneighbors liveCells cell
        neighborCount = (List.length neighbors)
        isLive = List.member cell liveCells
    in
      if isLive then []
      else rule4 neighborCount cell


type alias Automaton = 
  { liveCells : List (Int, Int)
  , deadCells : List (Int, Int)
  , liveRules : List (Int -> (Int, Int) -> Maybe (Int, Int))
  , deadRules : List (Int -> (Int, Int) -> Maybe (Int, Int))
  }

automataStep : Automaton -> Automaton
automataStep automaton = 
  { automaton | liveCells = automaton.liveCells } 

-- TODO: perhaps implement a cell type and complete the automatonStep function above

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 100 Step



-- VIEW


view : Model -> Html Msg
view model =
  let
      pauseButtonText = if model.paused then "Continue" else "Pause"
  in
    div []
      [ drawGrid <| List.map (\x -> (toFloat <| Tuple.first x, toFloat <| Tuple.second x)) model.liveCells
      , h3 [] [ text <| "Time Step: " ++ (String.fromInt model.timeStep) ]
      , button [ onClick <| Pause <| not model.paused ] [ text pauseButtonText ]
      ]


updateCells : List (Float, Float) -> Html.Html msg
updateCells = 
  let
    toSquare (a, b) =
      Svg.rect
        [ x (String.fromFloat (a * cellSize + 1.5))
        , y (String.fromFloat (b * cellSize + 1.5))
        , width <| String.fromInt (cellSize - 1)
        , height <| String.fromInt (cellSize - 1) 
        , fill "purple" ] []
  in
    Svg.svg [ width <| String.fromInt <| (gridWidth*(cellSize+1)) + 2
        , height <| String.fromInt <| (gridHeight*(cellSize+1)) + 2
        ]  << List.map toSquare


drawGrid : List (Float, Float) -> Html.Html msg
drawGrid cells = 
  let
      gridLineVert : Int -> Svg.Svg msg
      gridLineVert i =
        Svg.line
          [ x1 (String.fromInt (i * cellSize + 1))
          , y1 "1"
          , x2 (String.fromInt (i * cellSize + 1))
          , y2 <| String.fromInt <| (gridHeight*cellSize) + 1
          , stroke "lightgrey"
          , strokeWidth "1" ] []

      gridLineHori : Int -> Svg.Svg msg
      gridLineHori i =
        Svg.line
          [ x1 "1"
          , y1 (String.fromInt (i * cellSize + 1))
          , x2 <| String.fromInt <| (gridWidth*cellSize) + 1
          , y2 (String.fromInt (i * cellSize + 1))
          , stroke "lightgrey"
          , strokeWidth "1" ] []
  in
    Svg.svg [ width <| String.fromInt <| (gridWidth*(cellSize+1)) + 2
        , height <| String.fromInt <| (gridHeight*(cellSize+1)) + 2
        ] 
        ([ List.map gridLineVert <| List.range 0 gridWidth
         , List.map gridLineHori <| List.range 0 gridHeight
         , [ updateCells cells ]
         ] |> List.foldr (++) [])