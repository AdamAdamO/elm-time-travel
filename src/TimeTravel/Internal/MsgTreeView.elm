module TimeTravel.Internal.MsgTreeView exposing (view)

import TimeTravel.Internal.Styles as S
import TimeTravel.Internal.Util.RTree exposing (RTree(..))
import TimeTravel.Internal.Model exposing (HistoryItem, Id)
import TimeTravel.Internal.MsgLike as MsgLike

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Diff exposing (..)

import InlineHover exposing (hover)


view : (Id -> m) -> Id -> RTree (HistoryItem model msg) -> Html m
view onSelect selectedMsg tree =
  div
    (S.styles S.msgTreeView)
    (viewTree onSelect 0 selectedMsg tree)


viewTree : (Id -> m) -> Int -> Int -> RTree (HistoryItem model msg) -> List (Html m)
viewTree onSelect indent selectedMsg (Node item list) =
  itemRow onSelect indent selectedMsg item ::
    List.concatMap (viewTree onSelect (indent + 1) selectedMsg) list


itemRow : (Id -> m) -> Int -> Int -> HistoryItem model msg -> Html m
itemRow onSelect indent selectedMsg item =
  hover
    (S.msgTreeViewItemRowHover (selectedMsg == item.id))
    div
    ([ onClick (onSelect item.id)
    ] ++ S.styles (S.msgTreeViewItemRow (selectedMsg == item.id)))
    [ text (String.repeat indent "    " ++ String.fromInt item.id ++ ": " ++ MsgLike.format item.msg) ]
