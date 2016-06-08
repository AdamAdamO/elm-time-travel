module TimeTravel.Internal.Parser.Parser exposing (..) -- where

import String
import Parser exposing (..)
import Parser.Char exposing (braced)
-- import Parser.Number as PN

import TimeTravel.Internal.Parser.AST exposing (..)
import TimeTravel.Internal.Parser.Util exposing (..)


parse : String -> Result String AST
parse s = Parser.parse expression s


----

expression : Parser AST
expression =
  recursively (\_ ->
  spaced record
  `or` spaced stringLiteral
  `or` spaced value
  )

stringLiteral : Parser AST
stringLiteral =
  map StringLiteral <|
  (\_ s _ -> s)
  `map` symbol '"'
  `and` stringChars
  `and` symbol '"'


value : Parser AST
value =
  map (Value << String.trim) <|
    string (satisfy (\c -> c /= '=' && c /= '}' && c /= ','))

stringChars : Parser String
stringChars =
  string (satisfy (\c -> c /= '"'))


record : Parser AST
record =
  recursively (\_ ->
  map Record <| braced properties
  )

properties : Parser (List AST)
properties =
  recursively (\_ ->
  spaced (separatedBy property comma)
  )

propertyKey : Parser String
propertyKey =
  recursively (\_ ->
  string (satisfy (\c -> not (isSpace c) && c /= '='))
  )

property : Parser AST
property =
  recursively (\_ ->
  (\_ key _ _ _ value _ -> Property key value)
  `map` spaces
  `and` propertyKey
  `and` spaces
  `and` equal
  `and` spaces
  `and` expression
  `and` spaces
  )