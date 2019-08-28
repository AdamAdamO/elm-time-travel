module TimeTravel.Internal.Update exposing (update, updateAfterUserMsg)

import TimeTravel.Internal.Model exposing 
  (OutgoingMsg, Msg(..), Model, saveSetting
  , decodeSettings, selectFirstIfSync, futureToHistory
  , updateLazyAst, updateLazyDiff, updateLazyAstForWatch)
import Set exposing (Set)

update : (OutgoingMsg -> Cmd Never) -> Msg -> Model model msg -> (Model model msg, Cmd Msg)
update save message model =
  case message of
    Receive incomingMsg ->
      if incomingMsg.type_ == "load" then
        case decodeSettings incomingMsg.settings of
          Ok { fixedToLeft, filter } ->
            ({ model | fixedToLeft = fixedToLeft, filter = filter }, Cmd.none)

          Err error ->
            (model, Cmd.none) |> Debug.log "err decoding"
      else
        (model, Cmd.none)

    ToggleSync ->
      let
        nextSync = not model.sync
        newModel =
          { model |
            selectedMsg =
              if nextSync then
                Nothing
              else
                model.selectedMsg
          , sync = nextSync
          , showModelDetail = False
          }
          |> selectFirstIfSync
          |> if nextSync then futureToHistory else identity
      in
        (newModel, Cmd.none)

    ToggleExpand ->
      let
        newModel =
          { model | expand = not model.expand }
      in
        (newModel, Cmd.none)

    ToggleFilter name ->
      let
        newModel =
          { model |
            filter =
              List.map
                (\(name_, visible) ->
                  if name == name_ then
                    (name_, not visible)
                  else
                    (name_, visible)
                )
              model.filter
          }
      in
        (newModel, Cmd.batch [ saveSetting save newModel ])

    SelectMsg id ->
      let
        newModel =
          { model |
            selectedMsg = Just id
          , sync = False
          } |> updateLazyAst |> updateLazyDiff
      in
        (newModel, Cmd.none)

    Resync ->
      let
        newModel =
          { model |
            sync = True
          } |> selectFirstIfSync |> futureToHistory
      in
        (newModel, Cmd.none)

    ToggleLayout ->
      let
        newModel =
          { model |
            fixedToLeft = not model.fixedToLeft
          }
      in
        (newModel, Cmd.batch [ saveSetting save newModel ])

    ToggleModelDetail showModelDetail ->
        ( { model |
            showModelDetail = showModelDetail
          }
          |> updateLazyDiff
        , Cmd.none)

    ToggleModelTree id ->
      ({ model | expandedTree = toggleSet id model.expandedTree }, Cmd.none)

    ToggleMinimize ->
      ( { model |
          minimized = not model.minimized
        , sync = True
        }
        |> selectFirstIfSync
        |> futureToHistory
      , Cmd.none)

    InputModelFilter s ->
      ({ model | modelFilter = s }, Cmd.none)

    SelectModelFilter id ->
      ({ model | modelFilter = id }, Cmd.none)

    SelectModelFilterWatch id ->
      ( { model |
          modelFilter = id
        , watch = Just id
        }
        |> updateLazyAstForWatch
      , Cmd.none)

    StopWatching ->
      ({ model |
        watch = Nothing
      }, Cmd.none)


toggleSet : comparable -> Set comparable -> Set comparable
toggleSet a set =
  (if Set.member a set then Set.remove else Set.insert) a set


updateAfterUserMsg : (OutgoingMsg -> Cmd Never) -> Model model msg -> (Model model msg, Cmd Msg)
updateAfterUserMsg save model =
  (model, Cmd.batch [ saveSetting save model ])
