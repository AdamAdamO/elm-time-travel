module TimeTravel.Browser exposing
  ( sandbox
  , element
  , document
  , application
  , defaultConfig
  , TimeTravelConfig
  )


{-| Each functions in this module has the same interface as [Browser.App](https://package.elm-lang.org/packages/elm/browser/latest/Browser)

# Start your Program
@docs sandbox, element, document, application
@docs defaultConfig, TimeTravelConfig

-}


import TimeTravel.Internal.Model as Model exposing 
  ( Model, Msg(..)
  , OutgoingMsg, IncomingMsg
  , updateOnIncomingUserMsg
  )
import TimeTravel.Internal.Update as Update
import TimeTravel.Internal.View as View
import TimeTravel.Internal.Util.Nel as Nel

import Html exposing (Html)
import Browser
import Browser.Navigation exposing (Key)
import Url

{-| Record annotation if you want to please elm-format
-}
type alias TimeTravelConfig = 
  { startMinimized: Bool
  , startToLeft: Bool
  }

{-| Default configuration record
-}
defaultConfig: TimeTravelConfig
defaultConfig = 
  { startMinimized = False
  , startToLeft = False
  }

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


{-| See [Browser.sandbox](https://package.elm-lang.org/packages/elm/browser/latest/Browser#sandbox)
-}
sandbox :
  (model -> String)
  -> (msg -> String)
  -> TimeTravelConfig
  ->
  { init : model
  , view : model -> Html msg
  , update : msg -> model -> model
  }
  -> Program () (Model model msg) (Msg msg)
sandbox modelToString msgToString config { init, view, update } =
  let
    options =
      wrap
        { outgoingMsg = always Cmd.none
        , incomingMsg = always Sub.none
        , config = mergeConfig modelToString msgToString config
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


{-| See [Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element)
-}
element :
  (model -> String)
  -> (msg -> String)
  -> TimeTravelConfig
  ->
  { init : flags -> (model, Cmd msg)
  , view : model -> Html msg
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  }
  -> Program flags (Model model msg) (Msg msg)
element modelToString msgToString config {init, view, update, subscriptions} =
  let
    options =
      wrap
        { outgoingMsg = always Cmd.none
        , incomingMsg = always Sub.none
        , config = mergeConfig modelToString msgToString config
        }
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
  in    
    Browser.element
      { init = \flags -> options.init flags
      , view = options.view
      , update = options.update
      , subscriptions = options.subscriptions
      }


{-| See [Browser.document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document)
-}
document :
  (model -> String)
  -> (msg -> String)
  -> TimeTravelConfig
  ->
  { init : flags -> (model, Cmd msg)
  , view : model -> Browser.Document msg
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  }
  -> Program flags (Model model msg) (Msg msg)
document modelToString msgToString config stuff =
  let
    options = 
      { outgoingMsg = always Cmd.none
      , incomingMsg = always Sub.none
      , config = mergeConfig modelToString msgToString config
      }
  in    
    Browser.document (wrapDocument options stuff)


{-| See [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application)
-}
application :
  (model -> String)
  -> (msg -> String)
  -> TimeTravelConfig
  ->
  { init : flags -> Url.Url -> Key -> (model, Cmd msg)
  , view : model -> Browser.Document msg
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  , onUrlRequest : Browser.UrlRequest -> msg
  , onUrlChange : Url.Url -> msg
  }
  -> Program flags (Model model msg) (Msg msg)
application modelToString msgToString config stuff =
  let
    options = 
      { outgoingMsg = always Cmd.none
      , incomingMsg = always Sub.none
      , config = mergeConfig modelToString msgToString config
      }
  in    
    Browser.application (wrapApplication options stuff)

mergeConfig: (model -> String) -> (msg -> String)-> TimeTravelConfig -> Model.Config model msg
mergeConfig modelToString msgToString config = 
  { modelToString = modelToString
  , msgToString = msgToString
  , startMinimized = config.startMinimized
  , startToLeft = config.startToLeft
  }

wrap :
  { outgoingMsg : OutgoingMsg -> Cmd Never
  , incomingMsg : (IncomingMsg -> (Msg msg)) -> Sub (Msg msg)
  , config: Model.Config model msg
  }
  -> OptionsWithFlags flags model msg
  -> OptionsWithFlags flags (Model model msg) (Msg msg)
wrap { outgoingMsg, incomingMsg, config } { init, view, update, subscriptions } =
  let
    init_ flags =
      wrapInit config (init flags)

    update_ msg model =
      wrapUpdate config update outgoingMsg msg model

    view_ model =
      View.view config (\c -> UserMsg (Nothing, c)) DebuggerMsg view model

    subscriptions_ model =
      wrapSubscriptions subscriptions incomingMsg model

  in
    { init = init_
    , update = update_
    , view = view_
    , subscriptions = subscriptions_
    }


wrapDocument :
  { outgoingMsg : OutgoingMsg -> Cmd Never
  , incomingMsg : (IncomingMsg -> (Msg msg)) -> Sub (Msg msg)
  , config: Model.Config model msg
  }
  -> OptionsForDocument flags model msg
  -> OptionsForDocument flags (Model model msg) (Msg msg)
wrapDocument { outgoingMsg, incomingMsg, config } { init, view, update, subscriptions } =
  let
    init_ flags =
      wrapInit config (init flags)

    update_ msg model =
      wrapUpdate config update outgoingMsg msg model

    view_ model =
      View.document config (\c -> UserMsg (Nothing, c)) DebuggerMsg view model

    subscriptions_ model =
      wrapSubscriptions subscriptions incomingMsg model

  in
    { init = init_
    , update = update_
    , view = view_
    , subscriptions = subscriptions_
    }


wrapApplication :
  { outgoingMsg : OutgoingMsg -> Cmd Never
  , incomingMsg : (IncomingMsg -> (Msg msg)) -> Sub (Msg msg)
  , config: Model.Config model msg
  }
  -> OptionsForApplication flags model msg
  -> OptionsForApplication flags (Model model msg) (Msg msg)
wrapApplication { outgoingMsg, incomingMsg, config } { init, view, update, subscriptions, onUrlRequest, onUrlChange } =
  let
    init_ flags url key =
      wrapInit config (init flags url key)

    update_ msg model =
      wrapUpdate config update outgoingMsg msg model

    view_ model =
      View.document config (\c -> UserMsg (Nothing, c)) DebuggerMsg view model

    subscriptions_ model =
      wrapSubscriptions subscriptions incomingMsg model
      
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

wrapInit: Model.Config model msg -> (model, Cmd msg) -> ( Model model msg, Cmd (Msg msg))
wrapInit config (model, cmd) = 
  let
    model_ = Model.init model
    newModel = {model_
      | minimized = config.startMinimized
      , fixedToLeft = config.startToLeft
      }
  in
    (newModel, Cmd.batch [ Cmd.map (\msg -> UserMsg (Just 0, msg)) cmd ])

wrapUpdate: Model.Config model msg -> (msg -> model -> (model, Cmd msg)) -> (Model.OutgoingMsg -> Cmd Never) -> Msg msg -> Model model msg -> (Model model msg, Cmd (Msg msg))
wrapUpdate config update outgoingMsg msg model =
  let
    toUserMessage (id_, msg_) = UserMsg (Just id_, msg_)
  in
    case msg of
      UserMsg msgWithId ->
        let
          (m, c1) =
            updateOnIncomingUserMsg config toUserMessage update msgWithId model

          (m_, c2) =
            Update.updateAfterUserMsg outgoingMsg m
        in
          (m_, Cmd.batch [ c1, Cmd.map DebuggerMsg c2 ])

      DebuggerMsg msg_ ->
        let
          (m, c) =
            Update.update config outgoingMsg msg_ model
        in
          (m, Cmd.batch [ Cmd.map DebuggerMsg c ])

wrapSubscriptions: (model -> Sub msg) -> ((IncomingMsg -> (Msg msg)) -> Sub (Msg msg)) -> Model model msg -> Sub (Msg msg)
wrapSubscriptions subscriptions incomingMsg model =
  let
    item = Nel.head model.history
  in
    Sub.batch
      [ Sub.map (\c -> UserMsg (Nothing, c)) (subscriptions item.model)
      , incomingMsg (DebuggerMsg << Receive)
      ]
