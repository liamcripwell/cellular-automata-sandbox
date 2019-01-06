import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Tuple



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
  { dieFace : (Int, Int)
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model (1, 1)
  , Cmd.none
  )



-- UPDATE


type Msg
  = Roll
  | NewFace (Int, Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace (Random.pair (Random.int 1 6) (Random.int 1 6))
      )

    NewFace (newFace1, newFace2) ->
      ( Model (newFace1, newFace2)
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
    [ h1 [] [ text (String.fromInt <| Tuple.first model.dieFace)
            , text (String.fromInt <| Tuple.second model.dieFace) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]