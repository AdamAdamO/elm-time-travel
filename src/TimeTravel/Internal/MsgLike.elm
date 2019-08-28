module TimeTravel.Internal.MsgLike exposing 
  ( MsgLike(..)
  , format
  )


type MsgLike msg
  = Message msg
  | Init


format : MsgLike msg -> String
format msgLike =
  case msgLike of
    Message m -> Debug.toString m
    Init -> "[Init]"
