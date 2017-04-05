module ActionCable
    exposing
        ( ActionCable
        , initCable
        , ActionCableError(..)
        , onWelcome
        , onPing
        , onConfirm
        , onRejection
        , onDidReceiveData
        , withDebug
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
import ActionCable.Msg exposing (Msg(..), Message(..), Subscribable(..))


--


type alias ActionCableData msg =
    { url : String
    , status : CableStatus
    , onWelcome : Maybe (() -> msg)
    , onPing : Maybe (Int -> msg)
    , onConfirm : Maybe (Identifier -> msg)
    , onRejection : Maybe (Identifier -> msg)
    , onDidReceiveData : Maybe (Identifier -> JD.Value -> msg)
    , subs : Dict Identifier Subscription
    , debug : Bool
    }


type ActionCable msg
    = ActionCable (ActionCableData msg)


initCable : String -> ActionCable msg
initCable url =
    ActionCable
        { url = url
        , status = Disconnected
        , onWelcome = Nothing
        , onPing = Nothing
        , onConfirm = Nothing
        , onRejection = Nothing
        , onDidReceiveData = Nothing
        , subs = Dict.empty
        , debug = False
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



--


onWelcome : Maybe (() -> msg) -> ActionCable msg -> ActionCable msg
onWelcome maybeMsg =
    map (\cable -> { cable | onWelcome = maybeMsg })


onPing : Maybe (Int -> msg) -> ActionCable msg -> ActionCable msg
onPing maybeMsg =
    map (\cable -> { cable | onPing = maybeMsg })


onConfirm : Maybe (Identifier -> msg) -> ActionCable msg -> ActionCable msg
onConfirm maybeMsg =
    map (\cable -> { cable | onConfirm = maybeMsg })


onRejection : Maybe (Identifier -> msg) -> ActionCable msg -> ActionCable msg
onRejection maybeMsg =
    map (\cable -> { cable | onRejection = maybeMsg })


onDidReceiveData : Maybe (Identifier -> JD.Value -> msg) -> ActionCable msg -> ActionCable msg
onDidReceiveData maybeMsg =
    map (\cable -> { cable | onDidReceiveData = maybeMsg })


withDebug : Bool -> ActionCable msg -> ActionCable msg
withDebug bool =
    map (\cable -> { cable | debug = bool })



--


subscribeTo : Identifier -> ActionCable msg -> Result ActionCableError ( ActionCable msg, Cmd msg )
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


unsubscribeFrom : Identifier -> ActionCable msg -> Result ActionCableError ( ActionCable msg, Cmd msg )
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


update : Msg msg -> ActionCable msg -> ActionCable msg
update msg =
    case msg of
        Welcome ->
            map (\cable -> { cable | status = Connected })

        Confirm identifier ->
            setSubStatus identifier Subscribed

        Rejected identifier ->
            setSubStatus identifier SubscriptionRejected

        _ ->
            identity


perform : String -> List ( String, JE.Value ) -> Identifier -> ActionCable msg -> Result ActionCableError (Cmd msg)
perform action data identifier =
    activeChannel identifier
        >> Result.map (\c -> WebSocket.send (url c) (Encoder.perform action data identifier))


drop : Identifier -> ActionCable a -> ( ActionCable a, Cmd a )
drop identifier cable =
    ( removeSub identifier cable
    , if Maybe.withDefault False <| Maybe.map Subscription.isActive <| getSubscription identifier cable then
        WebSocket.send (url cable) (Encoder.unsubscribeFrom identifier)
      else
        Cmd.none
    )


activeChannel : Identifier -> ActionCable msg -> Result ActionCableError (ActionCable msg)
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


activeCable : ActionCable msg -> Result ActionCableError (ActionCable msg)
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


subscriptions : ActionCable msg -> Dict Identifier Subscription
subscriptions =
    extract >> .subs



--


extract : ActionCable msg -> ActionCableData msg
extract (ActionCable cable) =
    cable


map : (ActionCableData msg -> ActionCableData msg) -> ActionCable msg -> ActionCable msg
map f =
    extract >> f >> ActionCable


url : ActionCable msg -> String
url =
    extract >> .url


status : ActionCable msg -> CableStatus
status =
    extract >> .status


getSubscription : Identifier -> ActionCable msg -> Maybe Subscription
getSubscription identifier =
    subscriptions >> Dict.get identifier


removeSub : Identifier -> ActionCable msg -> ActionCable msg
removeSub identifier =
    map (\c -> { c | subs = Dict.remove identifier c.subs })


addSubscription : Identifier -> Subscription -> ActionCable msg -> ActionCable msg
addSubscription identifier newSubscription =
    map (\cable -> { cable | subs = Dict.insert identifier newSubscription cable.subs })


setSubStatus : Identifier -> SubscriptionStatus -> ActionCable msg -> ActionCable msg
setSubStatus identifier status =
    map
        (\cable ->
            { cable
                | subs = Dict.update identifier (Maybe.map (always status)) cable.subs
            }
        )


debug : ActionCable msg -> Bool
debug =
    extract >> .debug



--


{-| Listens for ActionCable messages and converts them into type `msg`
-}
listen : (Msg msg -> msg) -> ActionCable msg -> Sub msg
listen fn cable =
    (Sub.batch >> Sub.map (mapAll fn))
        [ internalMsgs cable
        , externalMsgs cable
        ]


mapAll : (Msg msg -> msg) -> Msg msg -> msg
mapAll fn internalMsg =
    case internalMsg of
        ExternalMsg msg ->
            msg

        _ ->
            fn internalMsg


actionCableMessages : ActionCable msg -> Sub (Maybe Message)
actionCableMessages cable =
    WebSocket.listen (url cable) decodeMessage


log : ActionCable msg -> a -> a
log cable =
    if (debug cable) then
        Debug.log "phx_message"
    else
        identity


decodeMessage : String -> Maybe Message
decodeMessage =
    parseJson >> Result.toMaybe


internalMsgs : ActionCable msg -> Sub (Msg msg)
internalMsgs cable =
    Sub.map (mapInternalMsgs cable) (actionCableMessages cable)


mapInternalMsgs : ActionCable msg -> Maybe Message -> Msg msg
mapInternalMsgs cable maybeMessage =
    case maybeMessage of
        Just message ->
            case message of
                WelcomeMessage ->
                    Welcome

                PingMessage int ->
                    Ping int

                ConfirmMessage id ->
                    Confirm id

                RejectedMessage id ->
                    Rejected id

                ReceiveDataMessage id value ->
                    ReceiveData id value

        --
        -- case (log cable "Phoenix Message" message).event of
        --     "phx_reply" ->
        --         handleInternalPhxReply cable message
        --
        --     "phx_error" ->
        --         ChannelErrored message.topic
        --
        --     "phx_close" ->
        --         ChannelClosed message.topic
        --
        --     _ ->
        --         NoOp
        Nothing ->
            NoOp



--
-- handleInternalPhxReply : ActionCable msg -> Message -> Msg msg
-- handleInternalPhxReply cable message =
--     let
--         msg =
--             Result.toMaybe (JD.decodeValue replyDecoder message.payload)
--                 |> andThen
--                     (\( status, response ) ->
--                         message.ref
--                             |> andThen
--                                 (\ref ->
--                                     Dict.get message.topic cable.channels
--                                         |> andThen
--                                             (\channel ->
--                                                 if status == "ok" then
--                                                     if ref == channel.joinRef then
--                                                         Just (ChannelJoined message.topic)
--                                                     else if ref == channel.leaveRef then
--                                                         Just (ChannelClosed message.topic)
--                                                     else
--                                                         Nothing
--                                                 else
--                                                     Nothing
--                                             )
--                                 )
--                     )
--     in
--         Maybe.withDefault NoOp msg


externalMsgs : ActionCable msg -> Sub (Msg msg)
externalMsgs cable =
    Sub.map (mapExternalMsgs cable) (actionCableMessages cable)


mapExternalMsgs : ActionCable msg -> Maybe Message -> Msg msg
mapExternalMsgs (ActionCable cable) maybeMessage =
    case maybeMessage of
        Just message ->
            let
                maybeCallback =
                    case message of
                        WelcomeMessage ->
                            cable.onWelcome
                                |> Maybe.map (\m -> m ())

                        PingMessage int ->
                            cable.onPing
                                |> Maybe.map (\m -> m int)

                        ConfirmMessage id ->
                            cable.onConfirm
                                |> Maybe.map (\m -> m id)

                        RejectedMessage id ->
                            cable.onRejection
                                |> Maybe.map (\m -> m id)

                        ReceiveDataMessage id value ->
                            cable.onDidReceiveData
                                |> Maybe.map (\m -> m id value)
            in
                Maybe.withDefault NoOp <| Maybe.map ExternalMsg maybeCallback

        Nothing ->
            NoOp



-- replyDecoder : JD.Decoder ( String, JD.Value )
-- replyDecoder =
--     JD.map2 (,)
--         (field "status" JD.string)
--         (field "response" JD.value)
--
--
-- handlePhxReply : ActionCable msg -> Message -> Msg msg
-- handlePhxReply cable message =
--     let
--         msg =
--             Result.toMaybe (JD.decodeValue replyDecoder message.payload)
--                 |> andThen
--                     (\( status, response ) ->
--                         message.ref
--                             |> andThen
--                                 (\ref ->
--                                     Dict.get ref cable.pushes
--                                         |> andThen
--                                             (\push ->
--                                                 case status of
--                                                     "ok" ->
--                                                         Maybe.map (\f -> (ExternalMsg << f) response) push.onOk
--
--                                                     "error" ->
--                                                         Maybe.map (\f -> (ExternalMsg << f) response) push.onError
--
--                                                     _ ->
--                                                         Nothing
--                                             )
--                                 )
--                     )
--     in
--         Maybe.withDefault NoOp msg
-- handleEvent : ActionCable msg -> Message -> Msg msg
-- handleEvent cable message =
--     case Dict.get ( message.event, message.topic ) cable.events of
--         Just payloadToMsg ->
--             ExternalMsg (payloadToMsg message.payload)
--
--         Nothing ->
--             NoOp
