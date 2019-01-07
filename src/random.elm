import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Tuple
import Svg 
import Svg.Attributes exposing (..)
import List.Extra exposing (..)


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
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model []
--   , Random.generate NewState <| randomCells 5
  , Random.generate NewState <| randomCells 5
  )



-- UPDATE

randomCell : Random.Generator (Int, Int)
randomCell = Random.pair (Random.int 0 (gridWidth-1)) (Random.int 0 (gridHeight-1))

randomCells : Int -> Random.Generator (List (Int, Int))
randomCells n = Random.list n randomCell

cartesian : List a -> List b -> List (a,b)
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs

rule4 cell =
  let
      liveNeighbours (x, y) = 
        cartesian (List.range (x-1) (x+1)) (List.range (y-1) (y+1))
  in
    liveNeighbours cell


type Msg
  = TimeStep
  | Test
  | NewState (List (Int, Int))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TimeStep ->
      ( model
      , Random.generate NewState <| randomCells 5
      )

    Test ->
      ( Model <| List.foldr (++) [] <| List.map rule4 model.liveCells
      , Cmd.none
      )

    NewState liveCells ->
      ( Model liveCells
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ drawGrid <| 
        List.map (\x -> (toFloat <| Tuple.first x, toFloat <| Tuple.second x)) model.liveCells
    , button [ onClick Test ] [ text "Time Step" ]
    ]


-- TODO: get rid of these global vars 
gridWidth = 125
gridHeight = 75
cellSize = 10 -- Note: cell's internal size will be (cellSize-1)*(cellSize-1)


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