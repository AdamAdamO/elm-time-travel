module TimeTravel.Browser exposing
  ( sandbox
  --, program
  -- , programWithOptions
--  , programWithFlags
  -- , programWithFlagsWithOptions
  -- , OutgoingMsg
  -- , IncomingMsg
  )


{-| Each functions in this module has the same interface as [Html.App](http://package.elm-lang.org/packages/elm-lang/html/latest/Html)

# Start your Program
@docs beginnerProgram, program, programWithFlags

-}


import TimeTravel.Internal.Model as Model exposing (..)
import TimeTravel.Internal.Update as Update
import TimeTravel.Internal.View as View
import TimeTravel.Internal.Util.Nel as Nel

import Html exposing (Html, div, text)
import Browser

type Msg msg
  = DebuggerMsg Model.Msg
  | UserMsg (Maybe Int, msg)


{- Alias for internal use -}
type alias OptionsWithFlags flags model msg =
  { init : flags -> (model, Cmd msg)
  , view : model -> Html msg
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  }

type alias OutgoingMsg = Model.OutgoingMsg
type alias IncomingMsg = Model.IncomingMsg


{-| See [Html.beginnerProgram](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#beginnerProgram)
-}
sandbox :
  { init : model
  , view : model -> Html msg
  , update : msg -> model -> model
  }
  -> Program () (Model model msg) (Msg msg)
sandbox { init, view, update } =
  let
    options =
      wrap
        { outgoingMsg = always Cmd.none
        , incomingMsg = always Sub.none
        }
        { init = always (init, Cmd.none)
        , view = view
        , update = \msg model_ -> (update msg model_, Cmd.none)
        , subscriptions = always Sub.none
        }
  in
    Browser.sandbox
      { init = Tuple.first (options.init ())
      , view = options.view
      , update = \msg model_ -> Tuple.first (options.update msg model_)
      }


{-| See [Html.programWithFlags](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#programWithFlags)
-}
element :
  { init : flags -> (model, Cmd msg)
  , view : model -> Html msg
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  }
  -> Program flags (Model model msg) (Msg msg)
element stuff =
  let
    options = 
      { outgoingMsg = always Cmd.none
      , incomingMsg = always Sub.none
      }
  in    
    Browser.element (wrap options stuff)


wrap :
  { outgoingMsg : OutgoingMsg -> Cmd Never
  , incomingMsg : (IncomingMsg -> (Msg msg)) -> Sub (Msg msg)
  }
  -> OptionsWithFlags flags model msg
  -> OptionsWithFlags flags (Model model msg) (Msg msg)
wrap { outgoingMsg, incomingMsg } { init, view, update, subscriptions } =
  let
    init_ flags =
      let
        (model, cmd) = init flags
      in
        (Model.init model, Cmd.batch [ Cmd.map (\msg -> UserMsg (Just 0, msg)) cmd ])

    update_ msg model =
      case msg of
        UserMsg msgWithId ->
          let
            (m, c1) =
              updateOnIncomingUserMsg (\(id, msg_) -> UserMsg (Just id, msg_)) update msgWithId model

            (m_, c2) =
              Update.updateAfterUserMsg outgoingMsg m
          in
            (m_, Cmd.batch [ c1, Cmd.map DebuggerMsg c2 ])

        DebuggerMsg msg_ ->
          let
            (m, c) =
              Update.update outgoingMsg msg_ model
          in
            (m, Cmd.batch [ Cmd.map DebuggerMsg c ])

    view_ model =
      View.view (\c -> UserMsg (Nothing, c)) DebuggerMsg view model

    subscriptions_ model =
      let
        item = Nel.head model.history
      in
        Sub.batch
          [ Sub.map (\c -> UserMsg (Nothing, c)) (subscriptions item.model)
          , incomingMsg (DebuggerMsg << Receive)
          ]

  in
    { init = init_
    , update = update_
    , view = view_
    , subscriptions = subscriptions_
    }