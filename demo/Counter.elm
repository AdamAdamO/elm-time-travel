module Counter exposing (main)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)

import TimeTravel.Browser as TimeTravel exposing (defaultConfig)

main =
  TimeTravel.sandbox Debug.toString Debug.toString defaultConfig
    { init = 0
    , view = view
    , update = update 
    }


view: Int -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (Debug.toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]


type Msg = Increment | Decrement


update: Msg -> Int -> Int
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1
