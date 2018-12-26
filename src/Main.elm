import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = 
  { rows : String
  , cols : String
  }

init : Model
init =
  Model "5" "5"


-- UPDATE

type Msg 
  = Rows String
  | Cols String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Rows rows ->
      { model | rows = rows }

    Cols cols ->
      { model | cols = cols }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Rows" model.rows Rows
    , viewInput "text" "Cols" model.cols Cols
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  div [] 
    [ text ("Number of " ++ p ++ ": ")
    , input [ type_ t, placeholder p, value v, onInput toMsg ] [] ]


viewValidation : Model -> Html msg
viewValidation model =
  div [] [ text ( "Grid Size: " ++ model.rows ++ "x" ++ model.cols) ]