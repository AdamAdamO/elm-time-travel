import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Posix)

import TimeTravel.Browser as TimeTravel


main =
  TimeTravel.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = Posix


init : () -> (Model, Cmd Msg)
init flags =
  (Time.millisToPosix 0, Cmd.none)



-- UPDATE


type Msg
  = Tick Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (newTime, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


-- VIEW


view : Model -> Html Msg
view model =
  let
    seconds = 
      String.fromInt (Time.toSecond Time.utc model * 6)

  in
    svg   
      [ viewBox "0 0 100 100", width "300px" ]
      [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
      , line 
        [ x1 "50", y1 "50"
        , x2 "50", y2 "10"
        , stroke "#023963"
        , transform ("rotate(" ++ seconds ++ ", 50, 50)") ] 
        []
      ]
