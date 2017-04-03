module ActionCable.Msg exposing (Msg(..))

-- stdlib imports

import Json.Decode as JD


-- local imports

import ActionCable.Identifier as ID


--


type Msg
    = Welcome
    | Ping Int
    | Error String
    | Confirm ID.Identifier
    | ReceiveData ID.Identifier JD.Value
    | Rejected ID.Identifier
