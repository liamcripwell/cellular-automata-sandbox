import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)

gridWidth = 7
gridHeight = 10
cellSize = 3

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


drawGrid = 
  let
      gridLineVert : Int -> Html.Html msg
      gridLineVert i =
        line
          [ x1 (String.fromInt (i * cellSize +1))
          , y1 "0"
          , x2 (String.fromInt (i * cellSize +1))
          , y2 <| String.fromInt <| (gridHeight*cellSize) +1
          , stroke "black"
          , strokeWidth "1" ] []
      gridLineHori : Int -> Html.Html msg
      gridLineHori i =
        line
          [ x1 "0"
          , y1 (String.fromInt (i * cellSize + 1))
          , x2 <| String.fromInt <| (gridWidth*cellSize) + 1
          , y2 (String.fromInt (i * cellSize +1))
          , stroke "black"
          , strokeWidth "1" ] []
  in
    svg [ width <| String.fromInt <| (gridWidth*cellSize) + (gridWidth+1)
        , height <| String.fromInt <| (gridHeight*cellSize) + (gridHeight+1) 
        ] 
        <| (List.map gridLineVert <| List.range 0 gridWidth)
          ++ (List.map gridLineHori <| List.range 0 gridHeight)


diagonalLine = List.map (\x -> (x, x)) (List.range 0 (gridWidth-1))

perimeter = List.map (\x -> (0, x)) (List.range 0 (gridHeight-1)) 
            ++ List.map (\x -> (x, 0)) (List.range 0 (gridWidth-1))
            ++ List.map (\x -> (gridWidth-1, x)) (List.range 0 (gridHeight-1))
            ++ List.map (\x -> (x, gridHeight-1)) (List.range 0 (gridWidth-1))


main =
  drawGrid
