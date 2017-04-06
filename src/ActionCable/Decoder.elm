module ActionCable.Decoder exposing (parseJson)

-- stdlib imports

import Json.Decode as JD


-- local imports

import ActionCable.Identifier as Identifier exposing (Identifier, newIdentifier)
import ActionCable.Msg exposing (Msg(..))


parseJson : String -> Result String Msg
parseJson string =
    JD.decodeString decoder string


decoder : JD.Decoder Msg
decoder =
    JD.oneOf
        [ welcomeDecoder
        , pingDecoder
        , confirmDecoder
        , receiveDataDecoder
        , rejectionDecoder
        ]


welcomeDecoder : JD.Decoder Msg
welcomeDecoder =
    JD.map (always <| Welcome)
        (JD.field "type" (typeIs "welcome"))


pingDecoder : JD.Decoder Msg
pingDecoder =
    JD.map2 (always Ping)
        (JD.field "type" (typeIs "ping"))
        (JD.field "message" JD.int)


confirmDecoder : JD.Decoder Msg
confirmDecoder =
    JD.map2 (always Confirm)
        (JD.field "type" (typeIs "confirm_subscription"))
        (JD.field "identifier" identifierDecoder)


receiveDataDecoder : JD.Decoder Msg
receiveDataDecoder =
    JD.map2 ReceiveData
        (JD.field "identifier" identifierDecoder)
        (JD.field "message" JD.value)


rejectionDecoder : JD.Decoder Msg
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
