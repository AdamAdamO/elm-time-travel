module TimeTravel.Internal.MsgTreeView exposing (view)

import TimeTravel.Internal.Styles as S
import TimeTravel.Internal.Util.RTree exposing (RTree(..))
import TimeTravel.Internal.Model exposing (HistoryItem, Id, Config)
import TimeTravel.Internal.MsgLike as MsgLike
import TimeTravel.Internal.InlineHover exposing (hover)

import Html exposing (Html, div, text)
import Html.Events exposing (onClick)


view : Config model msg -> (Id -> m) -> Id -> RTree (HistoryItem model msg) -> Html m
view config onSelect selectedMsg tree =
  div
    (S.styles S.msgTreeView)
    (viewTree config onSelect 0 selectedMsg tree)


viewTree : Config model msg -> (Id -> m) -> Int -> Int -> RTree (HistoryItem model msg) -> List (Html m)
viewTree config onSelect indent selectedMsg (Node item list) =
  itemRow config onSelect indent selectedMsg item ::
    List.concatMap (viewTree config onSelect (indent + 1) selectedMsg) list


itemRow : Config model msg -> (Id -> m) -> Int -> Int -> HistoryItem model msg -> Html m
itemRow config onSelect indent selectedMsg item =
  hover
    (S.msgTreeViewItemRowHover (selectedMsg == item.id))
    div
    ( onClick (onSelect item.id)
      :: S.styles (S.msgTreeViewItemRow (selectedMsg == item.id)))
    [ text (String.repeat indent "    " ++ String.fromInt item.id ++ ": " ++ MsgLike.format config.msgToString item.msg) ]
