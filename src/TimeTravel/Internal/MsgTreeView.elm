module TimeTravel.Internal.MsgTreeView exposing (view)

import TimeTravel.Internal.Styles as S
import TimeTravel.Internal.Util.RTree exposing (RTree(..))
import TimeTravel.Internal.Model exposing (HistoryItem, Id)
import TimeTravel.Internal.MsgLike as MsgLike
import TimeTravel.Internal.InlineHover exposing (hover)

import Html exposing (Html, div, text)
import Html.Events exposing (onClick)

import Diff exposing (..)



view : (Id -> m) -> Id -> (msg -> String) -> RTree (HistoryItem model msg) -> Html m
view onSelect selectedMsg msgToString tree =
  div
    (S.styles S.msgTreeView)
    (viewTree onSelect 0 selectedMsg msgToString tree)


viewTree : (Id -> m) -> Int -> Int -> (msg -> String) -> RTree (HistoryItem model msg) -> List (Html m)
viewTree onSelect indent selectedMsg msgToString (Node item list) =
  itemRow onSelect indent selectedMsg msgToString item ::
    List.concatMap (viewTree onSelect (indent + 1) selectedMsg msgToString) list


itemRow : (Id -> m) -> Int -> Int -> (msg -> String) -> HistoryItem model msg -> Html m
itemRow onSelect indent selectedMsg msgToString item =
  hover
    (S.msgTreeViewItemRowHover (selectedMsg == item.id))
    div
    ( onClick (onSelect item.id)
      :: S.styles (S.msgTreeViewItemRow (selectedMsg == item.id)))
    [ text (String.repeat indent "    " ++ String.fromInt item.id ++ ": " ++ MsgLike.format msgToString item.msg) ]
