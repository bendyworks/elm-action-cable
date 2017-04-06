module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode as JD
import Json.Encode as JE


--

import ActionCable
import ActionCable.Identifier as ID
import ActionCable.Msg as ACMsg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = CableMsg ACMsg.Msg
    | Subscribe Int
    | Unsubscribe Int
    | Drop Int
    | SendData Int
    | UpdateText Int String
    | OnWelcome ()
    | Pinged Int
    | SubscriptionConfirmed ID.Identifier
    | SubscriptionRejected ID.Identifier
    | HandleData ID.Identifier JD.Value


type alias Model =
    { channels : List Channel
    , cable : ActionCable.ActionCable Msg
    , error : Maybe String
    }


type alias Channel =
    { id : Int
    , input : String
    , output : List String
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    { channels = List.range 1 4 |> List.map (\id -> Channel id "" [] Nothing)
    , cable = initCable
    , error = Nothing
    }
        ! []


initCable : ActionCable.ActionCable Msg
initCable =
    ActionCable.initCable "ws://localhost:3000/cable/"
        |> ActionCable.withDebug True
        |> ActionCable.onWelcome (Just OnWelcome)
        |> ActionCable.onPing (Just Pinged)
        |> ActionCable.onConfirm (Just SubscriptionConfirmed)
        |> ActionCable.onRejection (Just SubscriptionRejected)
        |> ActionCable.onDidReceiveData (Just HandleData)


mapForChannelId : (Channel -> Channel) -> Int -> List Channel -> List Channel
mapForChannelId f int =
    List.map
        (\channel ->
            if channel.id == int then
                f channel
            else
                channel
        )


setChannelError : Maybe ActionCable.ActionCableError -> Channel -> Channel
setChannelError err channel =
    { channel | error = Maybe.map ActionCable.errorToString err }


setInput : String -> Channel -> Channel
setInput string channel =
    { channel | input = string }


subscribe : Int -> Model -> ( Model, Cmd Msg )
subscribe int model =
    case ActionCable.subscribeTo (identifier int) model.cable of
        Ok ( cable, cmd ) ->
            { model | cable = cable } ! [ cmd ]

        Err err ->
            let
                channels =
                    mapForChannelId (setChannelError (Just err)) int model.channels
            in
                { model | channels = channels } ! []


unsubscribe : Int -> Model -> ( Model, Cmd Msg )
unsubscribe int model =
    case ActionCable.unsubscribeFrom (identifier int) model.cable of
        Ok ( cable, cmd ) ->
            let
                channels =
                    mapForChannelId (setChannelError Nothing) int model.channels
            in
                { model | cable = cable, channels = channels } ! [ cmd ]

        Err err ->
            let
                channels =
                    mapForChannelId (setChannelError (Just err)) int model.channels
            in
                { model | channels = channels } ! []


identifier : Int -> ID.Identifier
identifier int =
    ID.newIdentifier "BoardChannel" [ ( "id", toString int ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnWelcome () ->
            model ! []

        Pinged int ->
            model ! []

        SubscriptionConfirmed id ->
            model ! []

        SubscriptionRejected id ->
            model ! []

        Drop id ->
            drop id model

        HandleData id value ->
            handleData id value model ! []

        CableMsg cableMsg ->
            ActionCable.update cableMsg model.cable
                |> (\( cable, cmd ) -> { model | cable = cable } ! [ cmd ])

        Subscribe int ->
            subscribe int model

        Unsubscribe int ->
            unsubscribe int model

        SendData int ->
            sendData int model

        UpdateText int string ->
            let
                channels =
                    mapForChannelId (setInput string) int model.channels
            in
                { model | channels = channels } ! []


handleConnected : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleConnected ( model, cmd ) =
    ( model, cmd )


handleData : ID.Identifier -> JD.Value -> Model -> Model
handleData id value model =
    let
        str =
            JD.decodeValue (JD.field "text" JD.string) value

        int =
            id
                |> Tuple.second
                |> List.filterMap
                    (\( k, v ) ->
                        if k == "id" then
                            Just v
                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.andThen (String.toInt >> Result.toMaybe)
    in
        case ( str, int ) of
            ( Ok s, Just int_ ) ->
                let
                    channels =
                        model.channels
                            |> mapForChannelId (addOutput s << setChannelError Nothing) int_
                in
                    { model
                        | channels = channels
                    }

            ( Err str, Just int_ ) ->
                let
                    channels =
                        model.channels
                            |> mapForChannelId (\c -> { c | error = Just str }) int_
                in
                    { model | channels = channels }

            _ ->
                { model | error = Just "Something bad happened!" }


addOutput : String -> Channel -> Channel
addOutput string channel =
    { channel | output = string :: channel.output }


subscriptionConfirmed : ID.Identifier -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
subscriptionConfirmed id ( model, cmd ) =
    ( model, cmd )


subscriptionRejected : ID.Identifier -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
subscriptionRejected id ( model, cmd ) =
    ( model, cmd )


sendData : Int -> Model -> ( Model, Cmd Msg )
sendData int model =
    let
        channel =
            model.channels
                |> List.filter (.id >> (==) int)
                |> List.head
    in
        case channel of
            Just c ->
                case ActionCable.perform "update" [ ( "text", JE.string c.input ) ] (identifier int) model.cable of
                    Ok cmd ->
                        let
                            channels =
                                model.channels
                                    |> mapForChannelId (setInput "" << setChannelError Nothing) int
                        in
                            { model | channels = channels } ! [ cmd ]

                    Err err ->
                        let
                            channels =
                                model.channels
                                    |> mapForChannelId (setInput "" << (setChannelError <| Just err)) int
                        in
                            { model | channels = channels } ! []

            Nothing ->
                { model | error = Just "Channel not found" } ! []


drop : Int -> Model -> ( Model, Cmd Msg )
drop int model =
    let
        ( cable, cmd ) =
            ActionCable.drop (identifier int) model.cable
    in
        ( { model | cable = cable }, cmd )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    ActionCable.listen CableMsg model.cable



--


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text <| "Cable status: " ++ (toString (ActionCable.status model.cable))
            , br [] []
            , text <| "App error: " ++ (Maybe.withDefault "(none)" model.error)
            ]
        , div [] <|
            List.map (viewChannel model.cable) model.channels
        ]


viewChannel : ActionCable.ActionCable Msg -> Channel -> Html Msg
viewChannel cable channel =
    let
        id =
            identifier channel.id

        status =
            cable
                |> ActionCable.getSubscription id
                |> Maybe.map toString
                |> Maybe.withDefault "Not Attempted"

        output =
            channel.output
                |> String.join "\n"
    in
        dl []
            [ dt [] [ text "id" ]
            , dd [] [ text <| toString channel.id ]
            , dt [] [ text "subscribed?" ]
            , dd [] [ text status ]
            , dt [] [ text "action" ]
            , dd []
                [ button [ onClick (Subscribe channel.id) ] [ text "subscribe" ]
                , button [ onClick (Unsubscribe channel.id) ] [ text "unsubscribe" ]
                , button [ onClick (Drop channel.id) ] [ text "drop" ]
                ]
            , dt [] [ text "input" ]
            , dd []
                [ input [ onInput <| UpdateText channel.id ] []
                , button [ onClick (SendData channel.id) ] [ text "send" ]
                ]
            , dt [] [ text "error" ]
            , dd [] [ text <| Maybe.withDefault "(none)" channel.error ]
            , dt [] [ text "output" ]
            , dd []
                [ pre [] [ text output ]
                ]
            ]
