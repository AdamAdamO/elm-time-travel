module TimeTravel.Internal.Parser.Util exposing 
  ( spaced, spaces, comma, equal
  )

import Combine exposing (Parser, between, regex, string)


spaced : Parser s a -> Parser s a
spaced p =
  between spaces spaces p


spaces : Parser s String
spaces = regex "[ ]*"


comma : Parser s String
comma = string ","


equal : Parser s String
equal = string "="
