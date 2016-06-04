module TimeTravel exposing (beginnerProgram, program) -- where

import TimeTravel.Model as Model exposing (..)
import TimeTravel.Update as Update
import TimeTravel.View as View
import TimeTravel.Util exposing (..)

import Html exposing (Html, div)
import Html.App as App


type Msg msg
  = DebuggerMsg Model.Msg
  | UserMsg msg


type alias BeginnerOptions model msg =
  { model : model
  , view : model -> Html msg
  , update : msg -> model -> model
  }


type alias Options model msg =
  { init : (model, Cmd msg)
  , view : model -> Html msg
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  }


beginnerProgram : BeginnerOptions model msg -> Program Never
beginnerProgram { model, view, update } =
  App.program <| wrap
    { init = (model, Cmd.none)
    , view = view
    , update = \msg model -> (update msg model, Cmd.none)
    , subscriptions = always Sub.none
    }


program : Options model msg -> Program Never
program =
  App.program << wrap


wrap : Options model msg -> Options (Model model msg) (Msg msg)
wrap { init, view, update, subscriptions } =
  let
    init' =
      Model.init (fst init)
      ! [ Cmd.map UserMsg (snd init) ]
    update' msg model =
      case msg of
        UserMsg msg ->
          let
            (Nel current past) = model.history
            (_, m) = current
            (newUserModel, userCmd) = update msg m
          in
            { model |
              history = Nel (Just msg, newUserModel) (current :: past)
            } ! [ Cmd.map UserMsg userCmd ]
        DebuggerMsg msg ->
          (Update.update msg model) ! []
    view' model =
      let
        (Nel (_, m) _) = model.history
      in
        view_ model (view m)
    subscriptions' model =
      let
        (Nel (_, m) _) = model.history
      in
        Sub.map UserMsg (subscriptions m)
  in
    { init = init'
    , update = update'
    , view = view'
    , subscriptions = subscriptions'
    }


view_ : Model model msg -> Html msg -> Html (Msg msg)
view_ model original =
  div
    []
    [ App.map UserMsg original
    , App.map DebuggerMsg (View.view model)
    ]
