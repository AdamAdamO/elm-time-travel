module DragAndDrop exposing (main)

import Html exposing (div, text, Html, Attribute)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (field)
import Browser.Events as Browser

import TimeTravel.Browser as TimeTravel exposing (defaultConfig)


main =
  TimeTravel.element Debug.toString Debug.toString defaultConfig
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Position =
  { x : Int
  , y : Int
  }

type alias Model =
    { position : Position
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


init : () -> ( Model, Cmd Msg )
init _ =
  ( Model (Position 200 200) Nothing, Cmd.none )



-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({position, drag} as model) =
  case msg of
    DragStart xy ->
      Model position (Just (Drag xy xy))

    DragAt xy ->
      Model position (Maybe.map (\{start} -> Drag start xy) drag)

    DragEnd _ ->
      Model (getPosition model) Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch 
        [ Browser.onMouseMove (Decode.map DragAt decodePosition)
        , Browser.onMouseUp (Decode.map DragEnd decodePosition)
        ]



-- VIEW


--(=>) = (,)


view : Model -> Html Msg
view model =
  let
    realPosition =
      getPosition model
  in
    div
      [ onMouseDown
      , style "background-color" "#3C8D2F"
      , style "cursor" "move"
      , style "width" "100px"
      , style "height" "100px"
      , style "border-radius" "4px"
      , style "position" "absolute"
      , style "left" (px realPosition.x)
      , style "top" (px realPosition.y)

      , style "color" "white"
      , style "display" "flex"
      , style "align-items" "center"
      , style "justify-content" "center"
      ]
      [ text "Drag Me!"
      ]


px : Int -> String
px number =
  String.fromInt number ++ "px"


getPosition : Model -> Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position

    Just {start,current} ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)


onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Decode.map DragStart decodePosition)

decodePosition : Decode.Decoder Position
decodePosition =
  Decode.map2 Position
    (Decode.field "pageX" Decode.int)
    (Decode.field "pageY" Decode.int)
