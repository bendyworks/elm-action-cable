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
    | Subscribe
    | SendData
    | HandleData ID.Identifier JD.Value
    | UpdateText String


type alias Model =
    { input : String
    , output : List String
    , cable : ActionCable.ActionCable
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    { input = ""
    , output = []
    , cable = ActionCable.initCable "ws://localhost:3000/cable/"
    , error = Nothing
    }
        ! []


subscribe : Model -> ( Model, Cmd Msg )
subscribe model =
    case ActionCable.subscribeTo identifier model.cable of
        Ok ( cable, cmd ) ->
            { model | cable = cable } ! [ Cmd.map CableMsg cmd ]

        Err err ->
            { model | error = Just <| ActionCable.errorToString err } ! []


identifier : ID.Identifier
identifier =
    ID.newIdentifier "BoardChannel" [ ( "id", "1" ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CableMsg cableMsg ->
            { model | cable = ActionCable.update cableMsg model.cable } ! []

        Subscribe ->
            subscribe model

        SendData ->
            sendData identifier model

        HandleData identifier value ->
            handleData identifier value model ! []

        UpdateText string ->
            { model | input = string } ! []


handleConnected : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleConnected ( model, cmd ) =
    ( model, cmd )


handleData : ID.Identifier -> JD.Value -> Model -> Model
handleData identifier value model =
    let
        str =
            JD.decodeValue (JD.field "text" JD.string) value
    in
        case str of
            Ok s ->
                { model | output = s :: model.output }

            Err str ->
                { model | error = Just str }


subscriptionConfirmed : ID.Identifier -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
subscriptionConfirmed identifier ( model, cmd ) =
    ( model, cmd )


subscriptionRejected : ID.Identifier -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
subscriptionRejected identifier ( model, cmd ) =
    ( model, cmd )


sendData : ID.Identifier -> Model -> ( Model, Cmd Msg )
sendData identifier model =
    case ActionCable.perform "update" [ ( "text", JE.string model.input ) ] identifier model.cable of
        Ok cmd ->
            { model | input = "" } ! [ cmd ]

        Err err ->
            { model | error = Just <| ActionCable.errorToString err } ! []



--


subscriptions : Model -> Sub Msg
subscriptions model =
    ActionCable.listen CableMsg HandleData model.cable



--


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick Subscribe ] [ text "subscribe" ]
            ]
        , input [ onInput UpdateText ] []
        , button [ onClick SendData ] [ text "submit" ]
        , pre [] <| List.map text <| List.intersperse "\n" model.output
        , div []
            [ text <| "errors: " ++ (Maybe.withDefault "(none)" model.error)
            ]
        ]
