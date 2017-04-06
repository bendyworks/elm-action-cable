module ActionCable.Identifier exposing (..)

{-|

# Types
@docs Identifier

# Constructors
@docs newIdentifier
-}


{-| Identifier for ActionCable. First item in the tuple is the channel
name, like `BoardChannel`. The second item is a list of `(String, String)`
tuples representing the params. For example, you might provide
`[("roomName", "general")]`
-}
type alias Identifier =
    ( String, List ( String, String ) )


{-| Construct a new Identifier.
-}
newIdentifier : a -> b -> ( a, b )
newIdentifier =
    (,)
