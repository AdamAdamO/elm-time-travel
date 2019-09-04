module TimeTravel.Internal.View exposing (view, document)

import TimeTravel.Internal.Model exposing 
  (Msg(..), Model, Id, FilterOptions, HistoryItem, Config
  , selectedItem, selectedMsgTree, selectedMsgAst
  )
import TimeTravel.Internal.MsgLike as MsgLike exposing (MsgLike(..))
import TimeTravel.Internal.Util.Nel as Nel
import TimeTravel.Internal.Styles as S
import TimeTravel.Internal.Icons as I
import TimeTravel.Internal.MsgTreeView as MsgTreeView
import TimeTravel.Internal.DiffView as DiffView
import TimeTravel.Internal.Parser.Formatter as Formatter
import TimeTravel.Internal.Parser.AST as AST exposing (ASTX)
import TimeTravel.Internal.InlineHover exposing (hover)

import Html exposing (Html, div, span, text, input, label)
import Html.Attributes exposing (type_, checked, placeholder, value, title)
import Html.Events exposing (onClick, onMouseDown, onInput)
import Html.Keyed as Keyed

import Set exposing (Set)
import Browser exposing (Document)

view : Config model msg -> (msg -> a) -> (Msg -> a) -> (model -> Html msg) -> Model model msg -> Html a
view config transformUserMsg transformDebuggerMsg userViewFunc model =
  div
    []
    [ Html.map transformUserMsg (userView userViewFunc model)
    , Html.map transformDebuggerMsg (debugView config model)
    ]

document : Config model msg -> (msg -> a) -> (Msg -> a) -> (model -> Document msg) -> Model model msg -> Document a
document config transformUserMsg transformDebuggerMsg userDocumentFunc model =
  let
    document_ = userDocument userDocumentFunc model
    body_ = List.map (Html.map transformUserMsg) document_.body
    debug_ = [ Html.map transformDebuggerMsg (debugView config model) ]
  in
    { title=document_.title
    , body = body_ ++ debug_
    }


userView : (model -> Html msg) -> Model model msg -> Html msg
userView userView_ model =
  case selectedItem model of
    Just item ->
      userView_ item.model

    Nothing ->
      text "Error: Unable to render"

userDocument : (model -> Document msg) -> Model model msg -> Document msg
userDocument userDocument_ model =
  case selectedItem model of
    Just item ->
      userDocument_ item.model

    Nothing -> 
      { title = "Error: Unable to render" 
      , body = [ text "Error: Unable to render" ]
      }


debugView : Config model msg -> Model model msg -> Html Msg
debugView config model =
  if model.minimized then minimizedDebugView model else normalDebugView config model


normalDebugView : Config model msg -> Model model msg -> Html Msg
normalDebugView config model =
  div
    []
    [ resyncView model.sync
    , div
        (S.styles (S.debugView model.fixedToLeft))
        [ headerView model.fixedToLeft model.sync model.expand model.filter
        , msgListView
            config
            model.filter
            model.selectedMsg
            (Nel.toList model.history)
            (watchView model)
            (detailView config model)
        ]
    ]


minimizedDebugView : Model model msg -> Html Msg
minimizedDebugView model =
  buttonView ToggleMinimize (S.minimizedButton model.fixedToLeft) [ I.minimize True ]


resyncView : Bool -> Html Msg
resyncView sync =
  if sync then
    text ""
  else
    div 
      ( onMouseDown Resync 
       :: S.styles (S.resyncView sync))
      []


headerView : Bool -> Bool -> Bool -> FilterOptions -> Html Msg
headerView fixedToLeft sync expand filterOptions =
  div []
  [ div 
    (S.styles S.headerView)
    [ buttonView ToggleLayout (S.buttonView True) [ I.layout ]
    , buttonView ToggleMinimize (S.buttonView True) [ I.minimize False ]
    , buttonView ToggleSync (S.buttonView False) [ I.sync sync ]
    , buttonView ToggleExpand (S.buttonView False) [ I.filterExpand expand ]
    ]
  , filterView expand filterOptions
  ]


buttonView : msg -> List (String, String) -> List (Html msg) -> Html msg
buttonView onClickMsg buttonStyle inner =
  hover S.buttonHover div 
    ( onClick onClickMsg     
      :: S.styles buttonStyle)
    inner


filterView : Bool -> FilterOptions -> Html Msg
filterView visible filterOptions =
  div
    (S.styles (S.filterView visible))
    (List.map filterItemView (List.sortBy Tuple.first filterOptions))


filterItemView : (String, Bool) -> Html Msg
filterItemView (name, visible) =
  div []
    [ label
        []
        [ input
            [ type_ "checkbox"
            , checked visible
            , onClick (ToggleFilter name)
            ]
            []
        , text name
        ]
    ]


modelDetailView : Bool -> String -> Set AST.ASTId -> Maybe (Result String ASTX) -> Config model msg -> model -> Html Msg
modelDetailView fixedToLeft modelFilter expandedTree lazyModelAst config userModel =
  case lazyModelAst of
    Just (Ok ast) ->
      let
        filterInput =
          modelFilterInput modelFilter

        filteredAst =
          if String.startsWith "@" modelFilter then
            case AST.filterByExactId modelFilter ast of
              Just x -> [(modelFilter, x)]
              Nothing -> []
          else
            AST.filterById modelFilter ast

        trees =
          List.map
            (\(id, ast_) ->
                modelDetailTreeEach
                  expandedTree
                  (if modelFilter /= "" then Just id else Nothing)
                  ast_
            )
            filteredAst

      in
        div 
          (S.styles (S.modelDetailView fixedToLeft))
          (filterInput :: trees)

    _ ->
      div 
        (S.styles S.modelView)
        [ text (config.modelToString userModel) ]


modelFilterInput : String -> Html Msg
modelFilterInput modelFilter =
  input
    ([ placeholder "Filter by property"
    , value modelFilter
    , onInput InputModelFilter
    ] ++ S.styles S.modelFilterInput)
    []


modelDetailTreeEach : Set AST.ASTId -> Maybe String -> ASTX -> Html Msg
modelDetailTreeEach expandedTree maybeId ast =
  let
    idView =
      case maybeId of
        Just id ->
          modelDetailTreeEachId id

        _ ->
          text ""
  in
    div
      (S.styles S.modelDetailTreeEach)
      ( idView ::
        Formatter.formatAsHtml
          SelectModelFilter
          ToggleModelTree
          expandedTree
          (Formatter.makeModel ast)
      )


modelDetailTreeEachId : String -> Html Msg
modelDetailTreeEachId id =
  let
    filterLink =
      hover
        S.modelDetailTreeEachIdHover
        span
        ( onClick (SelectModelFilter id)
          :: S.styles S.modelDetailTreeEachId)
        [ text id
        ]

    watchLink =
      hover
        S.modelDetailTreeEachIdWatchHover
        span
        ( onClick (SelectModelFilterWatch id)
          :: S.styles S.modelDetailTreeEachIdWatch)
        [ text "watch"
        ]
  in
    div
      []
      [ filterLink
      , span (S.styles S.modelDetailTreeEachIdWatch) [ text " (" ]
      , watchLink
      , span (S.styles S.modelDetailTreeEachIdWatch) [ text ")" ]
      ]


msgListView : Config model msg -> FilterOptions -> Maybe Id -> List (HistoryItem model msg) -> Html Msg -> Html Msg -> Html Msg
msgListView config filterOptions selectedMsg items watchView_ detailView_ =
  div
    []
    [ detailView_
    , watchView_
    , Keyed.node "div"
        (S.styles S.msgListView)
        ( filterMapUntilLimit 60 (msgView config filterOptions selectedMsg) items )
    ]


watchView : Model model msg -> Html Msg
watchView model =
  case (model.watch, (Nel.head model.history).lazyModelAst) of
    (Just id, Just (Ok ast)) ->
      let
        treeView =
          case AST.filterByExactId id ast of
            Just ast_ ->
              modelDetailTreeEach model.expandedTree Nothing ast_

            Nothing ->
              text ""
 
        stopWatchingButton =
          hover
            S.stopWatchingButtonHover
            div
            ( onClick StopWatching
              :: S.styles S.stopWatchingButton)
            [ I.stopWatching ]
      in
        div
          (S.styles S.watchView)
          [ div (S.styles S.watchViewHeader) [ text ("Watching " ++ id) ]
          , treeView
          , stopWatchingButton
          ]

    _ ->
      text ""


msgView : Config model msg -> FilterOptions -> Maybe Id -> (HistoryItem model msg) -> Maybe (String, Html Msg)
msgView config filterOptions selectedMsg { id, msg, causedBy } =
  let
    selected =
      case selectedMsg of
        Just msgId -> msgId == id
        Nothing -> False

    str =
      MsgLike.format config.msgToString msg

    visible =
      msg == Init ||
        case String.words str of
          tag :: _ ->
            List.any (\(name, visible_) -> tag == name && visible_) filterOptions
          _ ->
            False
  in
    if visible then
      Just (
        String.fromInt id
      , hover
          (S.msgViewHover selected)
          div
          ([ onClick (SelectMsg id)
          , title (String.fromInt id ++ ": " ++ str)
          ] ++ S.styles (S.msgView selected))
          [ text (String.fromInt id ++ ": " ++ str) ]
      )
    else
      Nothing


filterMapUntilLimit : Int -> (a -> Maybe b) -> List a -> List b
filterMapUntilLimit limit f list =
  List.reverse (filterMapUntilLimitHelp [] limit f list)


filterMapUntilLimitHelp : List b -> Int -> (a -> Maybe b) -> List a -> List b
filterMapUntilLimitHelp result limit f list =
  if limit <= 0 then
    result
  else
    case list of
      [] -> result
      h :: t ->
        case f h of
          Just b ->
            filterMapUntilLimitHelp (b :: result) (limit - 1) f t
          Nothing ->
            filterMapUntilLimitHelp result limit f t


detailView : Config model msg -> Model model msg -> Html Msg
detailView config model =
  if not model.sync then
    let
      msgTreeView =
        case (model.selectedMsg, selectedMsgTree model) of
          (Just id, Just tree) ->
            MsgTreeView.view config SelectMsg id tree
          _ ->
            text ""

      diffView =
        case selectedItem model of
          Just item ->
            case item.lazyDiff of
              Just changes ->
                DiffView.view changes
              Nothing ->
                text ""
          Nothing ->
            text ""

      detailedMsgView =
        case selectedMsgAst model of
          Just ast ->
            div
              (S.styles S.detailedMsgView)
              [ text (Formatter.formatAsString (Formatter.makeModel ast)) ]

          Nothing ->
            text ""

      head =
        div
          (S.styles S.detailViewHead)
          [ detailTab (S.detailTabModel model.fixedToLeft model.showModelDetail) (ToggleModelDetail True) "Model"
          , detailTab (S.detailTabDiff model.fixedToLeft (not model.showModelDetail)) (ToggleModelDetail False) "Messages and Diff"
          ]

      body =
        if model.showModelDetail then
          case selectedItem model of
            Just item ->
              [ modelDetailView
                model.fixedToLeft
                model.modelFilter
                model.expandedTree
                item.lazyModelAst
                config
                item.model ]
            _ ->
              []
        else
          [ msgTreeView
          , detailedMsgView
          , diffView
          ]

    in
      div
        (S.styles (S.detailView model.fixedToLeft True))
        ( head :: body )
  else
    text ""


detailTab : List (String, String) -> msg -> String -> Html msg
detailTab styles msg name =
  hover S.detailTabHover div 
    ( onClick msg 
      :: S.styles styles)
    [ text name ]
