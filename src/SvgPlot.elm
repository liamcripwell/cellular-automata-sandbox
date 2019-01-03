import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)

gridWidth = 100
gridHeight = 50
cellSize = 10

plotSquares : List (Int, Int) -> Html.Html msg
plotSquares =
  let
    toSquare (a, b) =
      rect
        [ x (String.fromInt (a * cellSize))
        , y (String.fromInt ((gridHeight*cellSize) - (b * cellSize)))
        , width <| String.fromInt cellSize
        , height <| String.fromInt cellSize ] []
  in
    svg [ width <| String.fromInt (gridWidth*cellSize)
        , height <| String.fromInt (gridHeight*cellSize) ] << List.map toSquare



diagonalLine = List.map (\x -> (x, x)) (List.range 1 (gridWidth-1))

perimeter = List.map (\x -> (1, x)) (List.range 1 (gridHeight-1)) 
            ++ List.map (\x -> (x, 1)) (List.range 1 (gridWidth-1))
            ++ List.map (\x -> (gridWidth-1, x)) (List.range 1 (gridHeight-1))
            ++ List.map (\x -> (x, gridHeight-1)) (List.range 1 (gridWidth-1))


main =
  plotSquares <| perimeter
