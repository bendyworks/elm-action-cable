module ActionCable.Decoder exposing (parseJson)

-- stdlib imports

import Json.Decode as JD


-- local imports

import ActionCable.Identifier as Identifier exposing (Identifier, newIdentifier)
import ActionCable.Msg exposing (Message(..))


parseJson : String -> Result String Message
parseJson string =
    JD.decodeString decoder string


decoder : JD.Decoder Message
decoder =
    JD.oneOf
        [ welcomeDecoder
        , pingDecoder
        , confirmDecoder
        , receiveDataDecoder
        , rejectionDecoder
        ]


welcomeDecoder : JD.Decoder Message
welcomeDecoder =
    JD.map (always <| WelcomeMessage)
        (JD.field "type" (typeIs "welcome"))


pingDecoder : JD.Decoder Message
pingDecoder =
    JD.map2 (always PingMessage)
        (JD.field "type" (typeIs "ping"))
        (JD.field "message" JD.int)


confirmDecoder : JD.Decoder Message
confirmDecoder =
    JD.map2 (always ConfirmMessage)
        (JD.field "type" (typeIs "confirm_subscription"))
        (JD.field "identifier" identifierDecoder)


receiveDataDecoder : JD.Decoder Message
receiveDataDecoder =
    JD.map2 ReceiveDataMessage
        (JD.field "identifier" identifierDecoder)
        (JD.field "message" JD.value)


rejectionDecoder : JD.Decoder Message
rejectionDecoder =
    JD.map2 (always RejectedMessage)
        (JD.field "type" (typeIs "reject_subscription"))
        (JD.field "identifier" identifierDecoder)


typeIs : String -> JD.Decoder String
typeIs typeName =
    let
        typeDecoder s =
            if s == typeName then
                JD.succeed typeName
            else
                JD.fail ""
    in
        JD.andThen
            typeDecoder
            JD.string


identifierDecoder : JD.Decoder Identifier
identifierDecoder =
    let
        extractIdentifier list =
            let
                ( channelList, params ) =
                    List.partition (\( key, _ ) -> key == "channel") list

                channel =
                    channelList
                        |> List.head
                        |> Maybe.map Tuple.second
            in
                case channel of
                    Just c ->
                        JD.succeed <| newIdentifier c params

                    Nothing ->
                        JD.fail "channel not specified"

        parseValue : String -> JD.Decoder (List ( String, String ))
        parseValue s =
            case JD.decodeString (JD.keyValuePairs JD.string) s of
                Ok ret ->
                    JD.succeed ret

                Err ret ->
                    JD.fail "failed to parse identifier string value"
    in
        JD.string
            |> JD.andThen parseValue
            |> JD.andThen extractIdentifier
