module ActionCable.Msg exposing (Msg(..), Message(..), Subscribable(..))

-- stdlib imports

import Json.Decode as JD


-- local imports

import ActionCable.Identifier as ID


--


type Msg msg
    = ExternalMsg msg
    | Welcome
    | Ping Int
    | Error String
    | Confirm ID.Identifier
    | ReceiveData ID.Identifier JD.Value
    | Rejected ID.Identifier
    | NoOp


type Message
    = WelcomeMessage
    | PingMessage Int
    | ConfirmMessage ID.Identifier
    | RejectedMessage ID.Identifier
    | ReceiveDataMessage ID.Identifier JD.Value


type Subscribable
    = WasWelcomed
    | WasPinged
    | WasConfirmed
    | WasRejected
    | DidReceiveData
