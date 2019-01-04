import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)

gridWidth = 50
gridHeight = 30
cellSize = 25 -- Note: cell's internal size will be (cellSize-1)*(cellSize-1)

plotSquares : List (Int, Int) -> Html.Html msg
plotSquares =
  let
    toSquare (a, b) =
      rect
        [ x (String.fromInt (a * cellSize))
        , y (String.fromInt (b * cellSize))
        , width <| String.fromInt cellSize
        , height <| String.fromInt cellSize ] []
  in
    svg [ width <| String.fromInt (gridWidth*cellSize)
        , height <| String.fromInt (gridHeight*cellSize) ] << List.map toSquare


drawGrid : Html.Html msg
drawGrid = 
  let
      gridLineVert : Int -> Svg msg
      gridLineVert i =
        line
          [ x1 (String.fromInt (i * cellSize + 1))
          , y1 "1"
          , x2 (String.fromInt (i * cellSize + 1))
          , y2 <| String.fromInt <| (gridHeight*cellSize) + 1
          , stroke "lightgrey"
          , strokeWidth "1" ] []
      gridLineHori : Int -> Svg msg
      gridLineHori i =
        line
          [ x1 "1"
          , y1 (String.fromInt (i * cellSize + 1))
          , x2 <| String.fromInt <| (gridWidth*cellSize) + 1
          , y2 (String.fromInt (i * cellSize + 1))
          , stroke "lightgrey"
          , strokeWidth "1" ] []
  in
    svg [ width <| String.fromInt <| (gridWidth*(cellSize+1)) + 2
        , height <| String.fromInt <| (gridHeight*(cellSize+1)) + 2
        ] 
        ([ (List.map gridLineVert <| List.range 0 gridWidth)
            ++ (List.map gridLineHori <| List.range 0 gridHeight)
        , [updateCells [(0, 0), (0, 1), (1, 0)]]
        ] |> List.foldr (++) [])


updateCells : List (Float, Float) -> Html.Html msg
updateCells = 
  let
    toSquare (a, b) =
      rect
        [ x (String.fromFloat (a * cellSize + 1.5))
        , y (String.fromFloat (b * cellSize + 1.5))
        , width <| String.fromInt (cellSize - 1)
        , height <| String.fromInt (cellSize - 1) ] []
  in
    svg [ width <| String.fromInt <| (gridWidth*(cellSize+1)) + 2
        , height <| String.fromInt <| (gridHeight*(cellSize+1)) + 2
        ]  << List.map toSquare


main = 
  Html.div [] 
    [ drawGrid
    , updateCells [(0, 0), (0, 1), (1, 0)]
    ]
