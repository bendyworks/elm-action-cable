module ActionCable.WireProtocol exposing (WireProtocol(..))

-- stdlib imports

import Json.Decode as JD


-- local imports

import ActionCable.Identifier as ID


--


type WireProtocol
    = Welcome
    | Ping Int
    | Error String
    | Confirm ID.Identifier
    | ReceiveData ID.Identifier JD.Value
    | Rejected ID.Identifier
