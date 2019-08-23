module TimeTravel.Browser exposing
  ( sandbox
  , element
  , document
  , application
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
import Browser.Navigation exposing (Key)
import Url

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

type alias OptionsForDocument flags model msg =
  { init : flags -> (model, Cmd msg)
  , view : model -> Browser.Document msg
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  }

type alias OptionsForApplication flags model msg =
  { init : flags -> Url.Url -> Key -> (model, Cmd msg)
  , view : model -> Browser.Document msg
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  , onUrlRequest : Browser.UrlRequest -> msg
  , onUrlChange : Url.Url -> msg
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


{-| See [Html.programWithFlags](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#programWithFlags)
-}
document :
  { init : flags -> (model, Cmd msg)
  , view : model -> Browser.Document msg
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  }
  -> Program flags (Model model msg) (Msg msg)
document stuff =
  let
    options = 
      { outgoingMsg = always Cmd.none
      , incomingMsg = always Sub.none
      }
  in    
    Browser.document (wrapDocument options stuff)


{-| See [Html.programWithFlags](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#programWithFlags)
-}
application :
  { init : flags -> Url.Url -> Key -> (model, Cmd msg)
  , view : model -> Browser.Document msg
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  , onUrlRequest : Browser.UrlRequest -> msg
  , onUrlChange : Url.Url -> msg
  }
  -> Program flags (Model model msg) (Msg msg)
application stuff =
  let
    options = 
      { outgoingMsg = always Cmd.none
      , incomingMsg = always Sub.none
      }
  in    
    Browser.application (wrapApplication options stuff)


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


wrapDocument :
  { outgoingMsg : OutgoingMsg -> Cmd Never
  , incomingMsg : (IncomingMsg -> (Msg msg)) -> Sub (Msg msg)
  }
  -> OptionsForDocument flags model msg
  -> OptionsForDocument flags (Model model msg) (Msg msg)
wrapDocument { outgoingMsg, incomingMsg } { init, view, update, subscriptions } =
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
      View.document (\c -> UserMsg (Nothing, c)) DebuggerMsg view model

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


wrapApplication :
  { outgoingMsg : OutgoingMsg -> Cmd Never
  , incomingMsg : (IncomingMsg -> (Msg msg)) -> Sub (Msg msg)
  }
  -> OptionsForApplication flags model msg
  -> OptionsForApplication flags (Model model msg) (Msg msg)
wrapApplication { outgoingMsg, incomingMsg } { init, view, update, subscriptions, onUrlRequest, onUrlChange } =
  let
    init_ flags url key =
      let
        (model, cmd) = init flags url key
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
      View.document (\c -> UserMsg (Nothing, c)) DebuggerMsg view model

    subscriptions_ model =
      let
        item = Nel.head model.history
      in
        Sub.batch
          [ Sub.map (\c -> UserMsg (Nothing, c)) (subscriptions item.model)
          , incomingMsg (DebuggerMsg << Receive)
          ]

    onUrlRequest_ url = 
      UserMsg (Nothing, onUrlRequest url)

    onUrlChange_ url = 
      UserMsg (Nothing, onUrlChange url)

  in
    { init = init_
    , update = update_
    , view = view_
    , subscriptions = subscriptions_
    , onUrlRequest = onUrlRequest_
    , onUrlChange = onUrlChange_
    }
