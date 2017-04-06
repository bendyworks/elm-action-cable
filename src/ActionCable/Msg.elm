module ActionCable.Msg exposing (Msg(..))

{-|

# Types
@ docs Msg

-}

-- stdlib imports

import Json.Decode as JD


-- local imports

import ActionCable.Identifier as ID


--


{-| The message types you might receive from the server.
-}
type Msg
    = Welcome
    | Ping Int
    | Confirm ID.Identifier
    | ReceiveData ID.Identifier JD.Value
    | Rejected ID.Identifier
    | NoOp
