module TimeTravel.Internal.DiffView exposing (view)

import TimeTravel.Internal.Styles as S exposing (styles)
import TimeTravel.Internal.Parser.AST exposing (ASTX)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Diff exposing (..)


type Line = Normal String | Delete String | Add String | Omit


lines : String -> List String
lines s =
  List.filter ((/=) "") <| String.lines s


view : List (Change String) -> Html msg
view changes =
  let
    linesView =
      List.map (\line ->
        case line of
          Normal s ->
            normalLine s

          Delete s ->
            deletedLine s

          Add s ->
            addedLine s

          Omit ->
            omittedLine
        ) (reduceLines changes)
  in
    div
      (styles S.diffView)
      linesView


reduceLines : List (Change String) -> List Line
reduceLines list =
  let
    additionalLines = 2

    (tmp, result) =
      List.foldr (\line (tmp_, result_) ->
        case line of
          NoChange s ->
            ((Normal s) :: tmp_, result_)

          Removed s ->
            tmpToResult additionalLines (Delete s) tmp_ result_

          Added s ->
            tmpToResult additionalLines (Add s) tmp_ result_
        ) ([], []) list
  in
    if result == [] then
      -- no change found
      []
    else if List.length tmp > additionalLines then
      Omit :: (List.drop (List.length tmp - additionalLines) tmp ++ result)
    else
      tmp ++ result


tmpToResult : Int -> Line -> List Line -> List Line -> (List Line, List Line)
tmpToResult additionalLines next tmp result =
  if result == [] then
    ([], next :: (List.take additionalLines tmp ++ (if List.length tmp > additionalLines then [ Omit ] else [])))
  else if List.length tmp > (additionalLines * 2) then
    ([], next :: (List.take additionalLines tmp ++ [Omit] ++ List.drop (List.length tmp - additionalLines) tmp ++ result))
  else
    ([], next :: (tmp ++ result))


omittedLine : Html msg
omittedLine =
  div (styles S.omittedLine) [ text "..." ]


deletedLine : String -> Html msg
deletedLine s =
  div (styles S.deletedLine) [ text s ]


addedLine : String -> Html msg
addedLine s =
  div (styles S.addedLine) [ text s ]


normalLine : String -> Html msg
normalLine s =
  div (styles S.normalLine) [ text s ]
