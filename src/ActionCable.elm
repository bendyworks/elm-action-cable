module ActionCable
    exposing
        ( ActionCable
        , CableStatus(..)
        , ActionCableError(..)
        , initCable
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
        , getSubscription
        , status
        , drop
        , perform
        , update
        , listen
        )

{-|

# Types
@docs ActionCable, CableStatus, ActionCableError

# Constructor
@docs initCable

# Callbacks/Configuration
@docs onWelcome, onPing, onConfirm, onRejection, onDidReceiveData, withDebug

# Outgoing Actions
@docs subscribeTo, unsubscribeFrom, perform

# Update helper
@docs update

# Subscriptions
@docs listen

# Helpers
@docs drop, errorToString, subscriptions, getSubscription, status

-}

-- stdlib imports

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Task
import WebSocket


-- local imports

import ActionCable.Decoder exposing (parseJson)
import ActionCable.Encoder as Encoder
import ActionCable.Identifier as Identifier exposing (Identifier, newIdentifier)
import ActionCable.Subscription as Subscription exposing (..)
import ActionCable.Msg exposing (Msg(..))


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


{-| Describes an ActionCable wrapper strtucture.
-}
type ActionCable msg
    = ActionCable (ActionCableData msg)


{-| Initialize an ActionCable with a URL.

Be sure the URL starts with "ws://" or "wss://".
-}
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


{-| Describes the connection status of the ActionCable.

**Note:** Currently, this will only go from `Disconnected` to `Connected`. It
will never go from `Connected` to `Disconnected`. This is a limitation with how
this library interfaces with the core WebSocket library, and it may be fixed in
a later version.
-}
type CableStatus
    = Disconnected
    | Connected


{-| Errors this library might raise.
-}
type ActionCableError
    = CableDisonnectedError
    | ChannelNotSubscribedError
    | AlreadySubscribedError
    | AlreadyTryingToSubscribeError
    | GeneralCableError String



--


{-| Hook for when ActionCable initially connects. Perhaps you'd want to
subscribe to a default channel that will help you bootstrap the rest of your
subscriptions.

    type Msg = SubscribeToInitialChannel () | ...

    initCable : ActionCable.ActionCable Msg
    initCable =
      ActionCable.initCable myUrl
        |> ActionCable.onWelcome (Just SubscribeToInitialChannel)

Pass `Nothing` if you previously subscribed to `onWelcome` but you don't want
to anymore (this will be rare).
-}
onWelcome : Maybe (() -> msg) -> ActionCable msg -> ActionCable msg
onWelcome maybeMsg =
    map (\cable -> { cable | onWelcome = log "onWelcome set to" cable maybeMsg })


{-| Hook for receiving pings every 3 seconds from the server. It's unlikely
you'll want to use this. The `Int` parameter is a timestamp.

Pass `Nothing` if you previously subscribed to `onPing` but you don't want
to anymore.
-}
onPing : Maybe (Int -> msg) -> ActionCable msg -> ActionCable msg
onPing maybeMsg =
    map
        (\cable -> { cable | onPing = log "onPing set to" cable maybeMsg })


{-| Hook for when your subscription to a channel is confirmed. See also
[`onRejection`](#onRejection).

    type Msg = SubscriptionConfirmed ID.Identifier | ...

    initCable : ActionCable.ActionCable Msg
    initCable =
      ActionCable.initCable myUrl
        |> ActionCable.onConfirm (Just SubscriptionConfirmed)

Pass `Nothing` if you previously subscribed to `onConfirm` but you don't want
to anymore.
-}
onConfirm : Maybe (Identifier -> msg) -> ActionCable msg -> ActionCable msg
onConfirm maybeMsg =
    map
        (\cable -> { cable | onConfirm = log "onConfirm set to" cable maybeMsg })


{-| Hook for when your subscription to a channel is rejected. See also
[`onConfirm`](#onConfirm).

    type Msg = SubscriptionRejected ID.Identifier | ...

    initCable : ActionCable.ActionCable Msg
    initCable =
      ActionCable.initCable myUrl
        |> ActionCable.onRejection (Just SubscriptionRejected)

Pass `Nothing` if you previously subscribed to `onRejection` but you don't want
to anymore.
-}
onRejection : Maybe (Identifier -> msg) -> ActionCable msg -> ActionCable msg
onRejection maybeMsg =
    map
        (\cable -> { cable | onRejection = log "onRejection set to" cable maybeMsg })


{-| Hook for receiving data. Almost definitely the most important hook you'll
want to use.

    type Msg = HandleData ID.Identifier Json.Decode.Value | ...

    initCable : ActionCable.ActionCable Msg
    initCable =
      ActionCable.initCable myUrl
        |> ActionCable.onDidReceiveData (Just HandleData)

Pass `Nothing` if you previously subscribed to `onDidReceiveData` but you don't
want to anymore.
-}
onDidReceiveData : Maybe (Identifier -> JD.Value -> msg) -> ActionCable msg -> ActionCable msg
onDidReceiveData maybeMsg =
    map
        (\cable -> { cable | onDidReceiveData = log "onDidReceiveData set to" cable maybeMsg })


{-| Turn on or off console debugging.

    initCable : ActionCable.ActionCable Msg
    initCable =
      ActionCable.initCable myUrl
        |> ActionCable.withDebug True
-}
withDebug : Bool -> ActionCable msg -> ActionCable msg
withDebug bool =
    map (\cable -> { cable | debug = Debug.log "[ActionCable] Debug set to" bool })



--


{-| Subscribe to a channel.

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
        case msg of
            SubscribeToRoom roomName ->
                ActionCable.subscribeTo
                    (ID.newIdentifier "ChatChannel" [("id", roomName)])
                    model.cable
                  |> Result.map (\(cable, cmd) -> ({model | cable = cable}, cmd))
                  |> Result.withDefault (model, Cmd.none) -- or actually handle the error
-}
subscribeTo : Identifier -> ActionCable msg -> Result ActionCableError ( ActionCable msg, Cmd msg )
subscribeTo identifier =
    let
        channelNotAlreadySubscribed identifier cable =
            case getSubscription identifier cable of
                Just SubscriptionAttempted ->
                    logg "Error before subscribing" cable <| Err AlreadyTryingToSubscribeError

                Just Subscribed ->
                    logg "Error before subscribing" cable <| Err AlreadySubscribedError

                _ ->
                    Ok cable
    in
        activeCable
            >> Result.andThen (channelNotAlreadySubscribed identifier)
            >> Result.map
                (\cable_ ->
                    ( addSubscription (logg "Attempting to subscribe to" cable_ identifier) newSubscription cable_
                    , WebSocket.send (extract cable_).url <| Encoder.subscribeTo identifier
                    )
                )


{-| Unsubscribe from a channel.

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
        case msg of
            UnsubscribeFrom roomName ->
                ActionCable.unsubscribeFrom
                    (ID.newIdentifier "ChatChannel" [("id", roomName)])
                    model.cable
                  |> Result.map (\(cable, cmd) -> ({model | cable = cable}, cmd))
                  |> Result.withDefault (model, Cmd.none) -- or actually handle the error
-}
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
                    logg "Error before unsubscribing" cable <| Err ChannelNotSubscribedError

        doUnsubscribe identifier cable_ =
            case getSubscription identifier cable_ of
                Just sub ->
                    Ok
                        ( setSubStatus identifier Unsubscribed cable_
                        , WebSocket.send (extract cable_).url <|
                            Encoder.unsubscribeFrom (logg "Attempting to unsubscribe from" cable_ identifier)
                        )

                Nothing ->
                    Err ChannelNotSubscribedError
    in
        activeCable
            >> Result.andThen (channelAlreadyUnsubscribed identifier)
            >> Result.andThen (doUnsubscribe identifier)


{-| Forward a raw message from the underlying [`WebSocket.listen`][wslisten] to
the ActionCable. If you've subscribed to any of the `on*` events like
[`onDidReceiveData`](#onDidReceiveData) or [`onWelcome`](#onWelcome), this
function will return a `Cmd` that will trigger your `Msg` on the next loop.

[wslisten]: http://package.elm-lang.org/packages/elm-lang/websocket/latest/WebSocket#listen
-}
update : Msg -> ActionCable msg -> ( ActionCable msg, Cmd msg )
update msg cable =
    let
        msgToCmd userCallback populate =
            cable
                |> (extract >> userCallback)
                |> Maybe.map (Task.succeed >> Task.perform populate)

        ( newCable, maybeCmd ) =
            case msg of
                Welcome ->
                    ( map (\cable -> { cable | status = Connected }) cable
                    , msgToCmd .onWelcome (\m -> m ())
                    )
                        |> qlog "Connected (Welcome!)" cable

                Confirm identifier ->
                    ( setSubStatus
                        (logg "Subscription Confirmed" cable identifier)
                        Subscribed
                        cable
                    , msgToCmd .onConfirm (\m -> m identifier)
                    )

                Rejected identifier ->
                    ( setSubStatus
                        (logg "Subscription Rejected" cable identifier)
                        SubscriptionRejected
                        cable
                    , msgToCmd .onRejection (\m -> m identifier)
                    )

                ReceiveData identifier value ->
                    let
                        _ =
                            logg "Data Received" cable ( identifier, value )
                    in
                        ( cable
                        , msgToCmd .onDidReceiveData (\m -> m identifier value)
                        )

                Ping int ->
                    let
                        _ =
                            logg "Ping Received" cable int
                    in
                        ( cable
                        , msgToCmd .onPing (\m -> m int)
                        )

                _ ->
                    ( cable, Nothing )
    in
        ( newCable
        , Maybe.withDefault Cmd.none maybeCmd
        )


{-| Perform an action on the Rails server. The `action` parameter is the name
of the action in the `ApplicationCable::Channel` subclass that you've
implemented. If you want to use a REST-like pattern, then `action` might be
`"index"` or `"update"`.

The second parameter is a list of `( String, Json.Encode.Value )` tuples, which
are the data you want to send to the `Channel` on the server. **Note:** Take
care not to provide `"action"` as one of the `String`s, as that will collide
with the `action` parameter.

The third paramter is an `Identifier`, which can be constructed with
`ActionCable.Identifier.newIdentifier`.
-}
perform : String -> List ( String, JE.Value ) -> Identifier -> ActionCable msg -> Result ActionCableError (Cmd msg)
perform action data identifier =
    let
        encoded =
            Encoder.perform action data identifier

        thisLog cable ret =
            logg "Sending" cable ( identifier, data )
                |> always ret
    in
        activeChannel identifier
            >> Result.map (\cable -> WebSocket.send (extract cable).url (thisLog cable encoded))


{-| Drop a `Subscription` from the internal list of subscriptions. If the
channel is currently subscribed, it will also send an "unsubscribe" message to
the server.

**Note:** This exists so that you can keep a rejected subscription around in
order to, perhaps, show an error message. To do that, with a delayed dismissal:

    update msg model =
        case msg of
            WasRejected identifier ->
                ( model
                , Task.perform
                    (always <| DismissRejection identifier)
                    (sleep (5 * seconds))
                )

            DismissRejection identifier ->
                ActionCable.drop identifier model.cable
                    |> (\(cable, cmd) -> ({ model | cable = cable}, cmd))
-}
drop : Identifier -> ActionCable a -> ( ActionCable a, Cmd a )
drop identifier cable =
    ( removeSub identifier cable
    , if Maybe.withDefault False <| Maybe.map Subscription.isActive <| getSubscription identifier cable then
        let
            id =
                logg "Unsubscribing and dropping channel" cable identifier
        in
            WebSocket.send (extract cable).url (Encoder.unsubscribeFrom id)
      else
        let
            _ =
                logg "Dropping channel" cable identifier
        in
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
                    logg "Error" cable <| Err ChannelNotSubscribedError
            )


{-| Convert an error value to a String. Write your own version if you'd like
to provide other (or perhaps localized) error messages.
-}
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
                    logg "Error" cable <| Err CableDisonnectedError

                Connected ->
                    Ok cable
    in
        Ok >> Result.andThen toActiveCable



--


extract : ActionCable msg -> ActionCableData msg
extract (ActionCable cable) =
    cable


map : (ActionCableData msg -> ActionCableData msg) -> ActionCable msg -> ActionCable msg
map f =
    extract >> f >> ActionCable


{-| Status of the `ActionCable`, either `Connected` or `Disconnected`.
-}
status : ActionCable msg -> CableStatus
status =
    extract >> .status



-- channel subscriptions


{-| List of subscriptions. See `Subscription`
-}
subscriptions : ActionCable msg -> Dict Identifier Subscription
subscriptions =
    extract >> .subs


{-| Maybe get one subscription.
-}
getSubscription : Identifier -> ActionCable msg -> Maybe Subscription
getSubscription identifier =
    subscriptions >> Dict.get identifier


setSubs : (Dict Identifier Subscription -> Dict Identifier Subscription) -> ActionCable msg -> ActionCable msg
setSubs f =
    map (\cable -> { cable | subs = f cable.subs })


removeSub : Identifier -> ActionCable msg -> ActionCable msg
removeSub identifier =
    setSubs <| Dict.remove identifier


addSubscription : Identifier -> Subscription -> ActionCable msg -> ActionCable msg
addSubscription identifier newSubscription =
    setSubs <| Dict.insert identifier newSubscription


setSubStatus : Identifier -> SubscriptionStatus -> ActionCable msg -> ActionCable msg
setSubStatus identifier status =
    setSubs <| Dict.update identifier (Maybe.map (always status))


{-| Listens for ActionCable messages and converts them into type `msg`

    import ActionCable.Msg as ACMsg

    type Msg
        = CableMsg ACMsg.Msg
        | ...

    subscriptions : Model -> Sub Msg
    subscriptions model =
        ActionCable.listen CableMsg model.cable
-}
listen : (Msg -> msg) -> ActionCable msg -> Sub msg
listen tagger cable =
    Sub.map tagger (actionCableMessages cable)


actionCableMessages : ActionCable msg -> Sub Msg
actionCableMessages cable =
    WebSocket.listen (extract cable).url decodeMessage


logg : String -> ActionCable msg -> a -> a
logg string (ActionCable cable) =
    log string cable


log : String -> ActionCableData msg -> a -> a
log string cable =
    if cable.debug then
        Debug.log <| "[ActionCable] " ++ string
    else
        identity


qlog : String -> ActionCable msg -> a -> a
qlog string (ActionCable cable) =
    let
        _ =
            if cable.debug then
                Debug.log "[ActionCable]" string
            else
                ""
    in
        identity


decodeMessage : String -> Msg
decodeMessage =
    parseJson >> Result.withDefault NoOp
