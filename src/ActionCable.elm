module ActionCable
    exposing
        ( ActionCable
        , initCable
        , ActionCableError(..)
        , Msg
        , errorToString
        , subscribeTo
        , unsubscribeFrom
        , subscriptions
        , drop
        , update
        , perform
        , listen
        )

-- stdlib imports

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import WebSocket


-- local imports

import ActionCable.Decoder exposing (parseJson)
import ActionCable.Encoder as Encoder
import ActionCable.Identifier as Identifier exposing (Identifier, newIdentifier)
import ActionCable.Subscription as Subscription exposing (..)
import ActionCable.Msg as ACMsg


--


type alias ActionCableData =
    { url : String
    , status : CableStatus
    , subs : Dict Identifier Subscription
    }


type ActionCable
    = ActionCable ActionCableData


initCable : String -> ActionCable
initCable url =
    ActionCable
        { url = url
        , status = Disconnected
        , subs = Dict.empty
        }


type CableStatus
    = Disconnected
    | Connected


type ActionCableError
    = CableDisonnectedError
    | ChannelNotSubscribedError
    | AlreadySubscribedError
    | AlreadyTryingToSubscribeError
    | GeneralCableError String


type alias Msg =
    ACMsg.Msg



--


subscribeTo : Identifier -> ActionCable -> Result ActionCableError ( ActionCable, Cmd a )
subscribeTo identifier =
    let
        channelNotAlreadySubscribed identifier cable =
            case getSubscription identifier cable of
                Just SubscriptionAttempted ->
                    Err AlreadyTryingToSubscribeError

                Just Subscribed ->
                    Err AlreadySubscribedError

                _ ->
                    Ok cable
    in
        activeCable
            >> Result.andThen (channelNotAlreadySubscribed identifier)
            >> Result.map
                (\cable_ ->
                    ( addSubscription identifier newSubscription cable_
                    , WebSocket.send (url cable_) <| Encoder.subscribeTo identifier
                    )
                )


unsubscribeFrom : Identifier -> ActionCable -> Result ActionCableError ( ActionCable, Cmd a )
unsubscribeFrom identifier =
    let
        channelAlreadyUnsubscribed identifier cable =
            case getSubscription identifier cable of
                Just SubscriptionAttempted ->
                    Ok cable

                Just Subscribed ->
                    Ok cable

                _ ->
                    Err ChannelNotSubscribedError

        doUnsubscribe identifier cable_ =
            case getSubscription identifier cable_ of
                Just sub ->
                    Ok
                        ( setSubStatus identifier Unsubscribed cable_
                        , WebSocket.send (url cable_) <| Encoder.unsubscribeFrom identifier
                        )

                Nothing ->
                    Err ChannelNotSubscribedError
    in
        activeCable
            >> Result.andThen (channelAlreadyUnsubscribed identifier)
            >> Result.andThen (doUnsubscribe identifier)


update : Msg -> ActionCable -> ActionCable
update msg =
    case msg of
        ACMsg.Welcome ->
            map (\cable -> { cable | status = Connected })

        ACMsg.Confirm identifier ->
            setSubStatus identifier Subscribed

        ACMsg.Rejected identifier ->
            setSubStatus identifier SubscriptionRejected

        _ ->
            identity


perform : String -> List ( String, JE.Value ) -> Identifier -> ActionCable -> Result ActionCableError (Cmd a)
perform action data identifier =
    activeChannel identifier
        >> Result.map (\c -> WebSocket.send (url c) (Encoder.perform action data identifier))


drop : Identifier -> ActionCable -> ( ActionCable, Cmd a )
drop identifier cable =
    ( removeSub identifier cable
    , if Maybe.withDefault False <| Maybe.map Subscription.isActive <| getSubscription identifier cable then
        WebSocket.send (url cable) (Encoder.unsubscribeFrom identifier)
      else
        Cmd.none
    )


activeChannel : Identifier -> ActionCable -> Result ActionCableError ActionCable
activeChannel identifier =
    activeCable
        >> Result.andThen
            (\cable ->
                if Maybe.withDefault False <| Maybe.map Subscription.isActive <| getSubscription identifier cable then
                    Ok cable
                else
                    Err ChannelNotSubscribedError
            )


errorToString : ActionCableError -> String
errorToString error =
    case error of
        CableDisonnectedError ->
            "Cable is disconnected. Please ensure you are calling `ActionCable.listen` in your subscriptions"

        ChannelNotSubscribedError ->
            "Channel was not subscribed to"

        AlreadySubscribedError ->
            "Channel is already subscribed to"

        AlreadyTryingToSubscribeError ->
            "Already trying to subscribe to channel"

        GeneralCableError str ->
            str


activeCable : ActionCable -> Result ActionCableError ActionCable
activeCable =
    let
        toActiveCable cable =
            case (status cable) of
                Disconnected ->
                    Err CableDisonnectedError

                Connected ->
                    Ok cable
    in
        Ok >> Result.andThen toActiveCable


subscriptions : ActionCable -> Dict Identifier Subscription
subscriptions =
    extract >> .subs



--


extract : ActionCable -> ActionCableData
extract (ActionCable cable) =
    cable


map : (ActionCableData -> ActionCableData) -> ActionCable -> ActionCable
map f =
    extract >> f >> ActionCable


url : ActionCable -> String
url =
    extract >> .url


status : ActionCable -> CableStatus
status =
    extract >> .status


getSubscription : Identifier -> ActionCable -> Maybe Subscription
getSubscription identifier =
    subscriptions >> Dict.get identifier


removeSub : Identifier -> ActionCable -> ActionCable
removeSub identifier =
    map (\c -> { c | subs = Dict.remove identifier c.subs })


addSubscription : Identifier -> Subscription -> ActionCable -> ActionCable
addSubscription identifier newSubscription =
    map (\cable -> { cable | subs = Dict.insert identifier newSubscription cable.subs })


setSubStatus : Identifier -> SubscriptionStatus -> ActionCable -> ActionCable
setSubStatus identifier status =
    map
        (\cable ->
            { cable
                | subs = Dict.update identifier (Maybe.map (always status)) cable.subs
            }
        )



-- subscriptions


listen : (Msg -> a) -> (Identifier -> JD.Value -> a) -> ActionCable -> Sub a
listen mapper dataHandler cable =
    WebSocket.listen (url cable) (listenHandler dataHandler mapper)


listenHandler : (Identifier -> JD.Value -> a) -> (Msg -> a) -> String -> a
listenHandler dataHandler mapper string =
    case parseJson string of
        Ok x ->
            case x of
                ACMsg.Welcome ->
                    mapper <| ACMsg.Welcome

                ACMsg.Ping int ->
                    mapper <| ACMsg.Ping int

                ACMsg.Confirm identifier ->
                    mapper <| ACMsg.Confirm identifier

                ACMsg.Rejected identifier ->
                    mapper <| ACMsg.Rejected identifier

                ACMsg.ReceiveData identifier data ->
                    dataHandler identifier data

                ACMsg.Error str ->
                    mapper <| ACMsg.Error str

        Err str ->
            mapper <| ACMsg.Error str
