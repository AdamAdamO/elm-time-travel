module Nav exposing (main)

import Html exposing (Html, h1, ul, li, text, a)
import Html.Attributes exposing (href)
import Browser
import Browser.Navigation as Navigation
import Url

import TimeTravel.Browser as TimeTravel exposing (defaultConfig)


config = {defaultConfig
  | msgToString = Debug.toString 
  , modelToString = Debug.toString
  }

main =
  --TimeTravel.program UrlChange
  TimeTravel.application config
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
init _ url key =
  let
    model = 
      { history = [ url ]
      , key = key
      }
  in
    (model, Cmd.none)



-- UPDATE


type Msg
  = UrlChange Url.Url
  | UrlRequest Browser.UrlRequest


{- We are just storing the url in our history in this example, but
normally, you would use a package like Url.Parser to parse the path
or hash into nicely structured Elm values.
    <https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation>
    <https://guide.elm-lang.org/webapps/navigation.html>
    <https://guide.elm-lang.org/webapps/url_parsing.html>
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
  li [] [ text (location.path ++ "#" ++ Maybe.withDefault "" location.fragment) ]
