import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import Browser.Navigation as Navigation
import Url

import TimeTravel.Browser as TimeTravel


main =
  --TimeTravel.program UrlChange
  TimeTravel.application
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    , onUrlChange = UrlChange
    , onUrlRequest = UrlRequest
    }



-- MODEL


type alias Model =
  { history : List Url.Url
  , key: Navigation.Key
  }


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model [ url ] key
  , Cmd.none
  )



-- UPDATE


type Msg
  = UrlChange Url.Url
  | UrlRequest Browser.UrlRequest


{- We are just storing the location in our history in this example, but
normally, you would use a package like evancz/url-parser to parse the path
or hash into nicely structured Elm values.
    <http://package.elm-lang.org/packages/evancz/url-parser/latest>
-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UrlChange location ->
      ( { model | history = location :: model.history }
      , Cmd.none
      )
    UrlRequest urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Navigation.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Navigation.load href )



-- VIEW


view : Model -> Browser.Document msg
view model =
  { title = "Nav.elm"
  , body =
      [ h1 [] [ text "Pages" ]
      , ul [] (List.map viewLink [ "bears", "cats", "dogs", "elephants", "fish" ])
      , h1 [] [ text "History" ]
      , ul [] (List.map viewLocation model.history)
      ]
  }


viewLink : String -> Html msg
viewLink name =
  li [] [ a [ href ("#" ++ name) ] [ text name ] ]


viewLocation : Url.Url -> Html msg
viewLocation location =
  li [] [ text (location.path ++ Maybe.withDefault "" location.fragment) ]
