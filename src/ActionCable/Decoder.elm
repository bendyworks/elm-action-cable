module ActionCable.Decoder exposing (parseJson)

-- stdlib imports

import Json.Decode as JD


-- local imports

import ActionCable.Identifier as Identifier exposing (Identifier, newIdentifier)
import ActionCable.WireProtocol exposing (WireProtocol(..))


parseJson : String -> Result String WireProtocol
parseJson string =
    JD.decodeString decoder string


decoder : JD.Decoder WireProtocol
decoder =
    JD.oneOf
        [ welcomeDecoder
        , pingDecoder
        , confirmDecoder
        , receiveDataDecoder
        , rejectionDecoder
        ]


welcomeDecoder : JD.Decoder WireProtocol
welcomeDecoder =
    JD.map (always Welcome)
        (JD.field "type" (typeIs "welcome"))


pingDecoder : JD.Decoder WireProtocol
pingDecoder =
    JD.map2 (always Ping)
        (JD.field "type" (typeIs "ping"))
        (JD.field "message" JD.int)


confirmDecoder : JD.Decoder WireProtocol
confirmDecoder =
    JD.map2 (always Confirm)
        (JD.field "type" (typeIs "confirm_subscription"))
        (JD.field "identifier" identifierDecoder)


receiveDataDecoder : JD.Decoder WireProtocol
receiveDataDecoder =
    JD.map2 ReceiveData
        (JD.field "identifier" identifierDecoder)
        (JD.field "message" JD.value)


rejectionDecoder : JD.Decoder WireProtocol
rejectionDecoder =
    JD.map2 (always Rejected)
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
