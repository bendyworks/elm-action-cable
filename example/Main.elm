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
    = CableMsg (ACMsg.Msg Msg)
    | Subscribe Int
    | SendData
    | UpdateText String
    | OnWelcome ()
    | Pinged Int
    | SubscriptionConfirmed ID.Identifier
    | SubscriptionRejected ID.Identifier
    | HandleData ID.Identifier JD.Value


type alias Model =
    { input : String
    , output : List String
    , cable : ActionCable.ActionCable Msg
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    { input = ""
    , output = []
    , cable = initCable
    , error = Nothing
    }
        ! []


initCable : ActionCable.ActionCable Msg
initCable =
    ActionCable.initCable "ws://localhost:3000/cable/"
        |> ActionCable.onWelcome (Just OnWelcome)
        |> ActionCable.onPing (Just Pinged)
        |> ActionCable.onConfirm (Just SubscriptionConfirmed)
        |> ActionCable.onRejection (Just SubscriptionRejected)
        |> ActionCable.onDidReceiveData (Just HandleData)
        |> ActionCable.withDebug True


subscribe : Int -> Model -> ( Model, Cmd Msg )
subscribe int model =
    case ActionCable.subscribeTo (identifier int) model.cable of
        Ok ( cable, cmd ) ->
            { model | cable = cable } ! [ cmd ]

        Err err ->
            { model | error = Just <| ActionCable.errorToString err } ! []


identifier : Int -> ID.Identifier
identifier int =
    ID.newIdentifier "BoardChannel" [ ( "id", toString int ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnWelcome _ ->
            model ! []

        Pinged int ->
            model ! []

        SubscriptionConfirmed id ->
            model ! []

        SubscriptionRejected id ->
            model ! []

        HandleData id value ->
            handleData id value model ! []

        CableMsg cableMsg ->
            { model | cable = ActionCable.update cableMsg model.cable } ! []

        Subscribe int ->
            subscribe int model

        SendData ->
            sendData (identifier 1) model

        UpdateText string ->
            { model | input = string } ! []


handleConnected : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleConnected ( model, cmd ) =
    ( model, cmd )


handleData : ID.Identifier -> JD.Value -> Model -> Model
handleData id value model =
    let
        str =
            JD.decodeValue (JD.field "text" JD.string) value
    in
        case str of
            Ok s ->
                { model | output = s :: model.output, error = Nothing }

            Err str ->
                { model | error = Just str }


subscriptionConfirmed : ID.Identifier -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
subscriptionConfirmed id ( model, cmd ) =
    ( model, cmd )


subscriptionRejected : ID.Identifier -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
subscriptionRejected id ( model, cmd ) =
    ( model, cmd )


sendData : ID.Identifier -> Model -> ( Model, Cmd Msg )
sendData id model =
    case ActionCable.perform "update" [ ( "text", JE.string model.input ) ] id model.cable of
        Ok cmd ->
            { model | input = "", error = Nothing } ! [ cmd ]

        Err err ->
            { model | error = Just <| ActionCable.errorToString err } ! []



--


subscriptions : Model -> Sub Msg
subscriptions model =
    ActionCable.listen CableMsg model.cable



--


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick (Subscribe 1) ] [ text "subscribe" ]
            , button [ onClick (Subscribe 2) ] [ text "subscribe (fail)" ]
            ]
        , input [ onInput UpdateText ] []
        , button [ onClick SendData ] [ text "submit" ]
        , pre [] <| List.map text <| List.intersperse "\n" model.output
        , div []
            [ text <| "errors: " ++ (Maybe.withDefault "(none)" model.error)
            ]
        ]
