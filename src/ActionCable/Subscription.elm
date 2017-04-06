module ActionCable.Subscription exposing (..)

{-|

# Types
@docs Subscription, SubscriptionStatus

# Constructor
@docs newSubscription

# Helper
@docs isActive
-}


{-| The subscription type.
-}
type alias Subscription =
    SubscriptionStatus


{-| Construct a new subscription (SubscriptionAttempted)
-}
newSubscription : Subscription
newSubscription =
    SubscriptionAttempted


{-| The various states of a subscription.
-}
type SubscriptionStatus
    = SubscriptionAttempted
    | Subscribed
    | SubscriptionRejected
    | Unsubscribed


{-| True if the subscription is currently being attempted or is
successfully subscribed.
-}
isActive : Subscription -> Bool
isActive subscription =
    case subscription of
        SubscriptionAttempted ->
            True

        Subscribed ->
            True

        _ ->
            False
