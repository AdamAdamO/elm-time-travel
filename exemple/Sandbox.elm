module Sandbox exposing (main)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import TimeTravel.Browser exposing (defaultConfig)


config = {defaultConfig
  | msgToString = Debug.toString
  , modelToString = Debug.toString
  , startMinimized = True
  }

main =
--  Browser.sandbox { init = 0, update = update, view = view }
  TimeTravel.Browser.sandbox config
    { init = init
    , update = update
    , view = view 
    }

type Msg = Increment Int | Decrement Int | Message String

type alias Model =
  { counter: Int
  , message: String
  , lastMsg: Msg
  }

init = 
  { counter = 0
  , message = ""
  , lastMsg = Increment 0
  }

update: Msg -> Model -> Model
update msg model =
  case msg of
    Increment value ->
      {model
      |counter = model.counter + value
      ,lastMsg = msg
      }

    Decrement value ->
      {model
      |counter = model.counter - value
      ,lastMsg = msg
      }

    Message value ->
      {model
      |message = value
      ,lastMsg = msg
      }

view: Model -> Html Msg
view model =
  div []
    [ button [ onClick (Decrement 1) ] [ text "-" ]
    , div [] [ text (String.fromInt model.counter) ]
    , button [ onClick (Increment 1)] [ text "+" ]
    , input [ onInput Message ] []
    ]

