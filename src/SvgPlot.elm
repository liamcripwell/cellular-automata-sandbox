import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)

plotSquares : List (Int, Int) -> Html.Html msg
plotSquares =
  let
    toSquare (a, b) =
      rect
        [ x (String.fromInt (a * 10))
        , y (String.fromInt (100 - (b * 10)))
        , width "10"
        , height "10" ] []
  in
    svg [ width "100", height "100" ] << List.map toSquare

main =
  plotSquares <| List.map (\x -> (x, x)) (List.range 1 9)