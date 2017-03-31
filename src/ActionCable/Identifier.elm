module ActionCable.Identifier exposing (..)


type alias Identifier =
    ( String, List ( String, String ) )


newIdentifier : a -> b -> ( a, b )
newIdentifier =
    (,)
