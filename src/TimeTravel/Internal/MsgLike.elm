module TimeTravel.Internal.MsgLike exposing 
  ( MsgLike(..)
  , format
  )


type MsgLike msg
  = Message msg
  | Init


format : (msg -> String) -> MsgLike msg -> String
format msgToString msgLike =
  case msgLike of
    Message m -> msgToString m
    Init -> "[Init]"
