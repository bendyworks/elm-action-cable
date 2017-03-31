module ActionCable.Encoder exposing (subscribeTo, unsubscribeFrom, perform)

-- stdlib imports

import Json.Encode as JE
import Tuple


-- local imports

import ActionCable.Identifier as Identifier exposing (Identifier)


subscribeTo : Identifier -> String
subscribeTo identifier =
    jsonObjectToString <|
        List.map (Tuple.mapSecond JE.string) <|
            [ ( "identifier", encodeIdentifier identifier )
            , ( "command", "subscribe" )
            ]


unsubscribeFrom : Identifier -> String
unsubscribeFrom identifier =
    jsonObjectToString <|
        List.map (Tuple.mapSecond JE.string) <|
            [ ( "identifier", encodeIdentifier identifier )
            , ( "command", "unsubscribe" )
            ]


perform : String -> List ( String, JE.Value ) -> Identifier -> String
perform action data identifier =
    let
        encodedIdentifier =
            encodeIdentifier identifier

        encodedData =
            jsonObjectToString <|
                ( "action", JE.string action )
                    :: data
    in
        jsonObjectToString <|
            [ ( "command", JE.string "message" )
            , ( "identifier", JE.string encodedIdentifier )
            , ( "data", JE.string encodedData )
            ]


jsonObjectToString : List ( String, JE.Value ) -> String
jsonObjectToString =
    JE.encode 0 << JE.object


encodeIdentifier : Identifier -> String
encodeIdentifier =
    jsonObjectToString << identifierValue


identifierValue : Identifier -> List ( String, JE.Value )
identifierValue ( channel, params ) =
    List.map (Tuple.mapSecond JE.string) (( "channel", channel ) :: params)
