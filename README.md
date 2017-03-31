# ActionCable in Elm

This package is an Elm client library for [ActionCable][ActionCable guide], which comes bundled with Ruby on Rails.

## Basic Usage

```elm
type Msg
    = SubscribeTo String
    | UnsubscribeFrom String
    | HandleData ActionCable.Identifier Json.Decode.Value
    | SendData String String
    | CableMsg ActionCable.WireProtocol



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubscribeTo roomName ->
            case ActionCable.subscribeTo (channelId roomName) model.cable of
                Ok ( cable, cmd ) ->
                    { model | cable = cable } ! [ Cmd.map CableMsg cmd ]

                Err err ->
                    -- you're probably already subscribed to this channel
                    { model | errorPopup = Just <| ActionCable.errorToString err } ! []

        UnsubscribeFrom roomName ->
            case ActionCable.unsubscribeFrom (channelId roomName) model.cable of
                Ok ( cable, cmd ) ->
                    { model | cable = cable } ! [ Cmd.map CableMsg cmd ]

                Err err ->
                    -- you're probably already unsubscribed from this channel
                    { model | errorPopup = Just <| ActionCable.errorToString err } ! []

        HandleData identifier value ->
            -- value is a Json.Decode.Value. You'll probably want more than `toString`
            { model | messages = toString value :: model.messages } ! []

        SendData roomName aStringToSend ->
            let
                sendCmd =
                    ActionCable.perform
                        "some_channel_action_name"
                        [ ( "jsonKey", Json.Encode.string aStringToSend ) ]
                        (channelId roomName)
                        model.cable
            in
                case sendCmd of
                    Ok toSend ->
                        ( model, toSend )

                    Err err ->
                        -- probably because you haven't subscribed to the channel yet
                        { model | errorPopup = Just <| ActionCable.errorToString err } ! []

        CableMsg cableMsg ->
            -- important to forward on "accounting" messages to the underlying submodel
            { model | cable = ActionCable.update cableMsg model.cable } ! []


channelId : String -> ActionCable.Identifier.Identifier
channelId roomName =
    ID.newIdentifier "ChatChannel" [ ( "room", roomName ) ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    ActionCable.listen CableMsg HandleData model.cable
```


## License

`elm-action-cable` is released under the Apache v2 license, the details of which can be found in the LICENSE file.

## Author

* [Brad Grzesiak](https://twitter.com/listrophy), [Bendyworks](http://bendyworks.com)


[ActionCable guide]: http://guides.rubyonrails.org/action_cable_overview.html "ActionCable guide"
