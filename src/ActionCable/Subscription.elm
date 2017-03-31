module ActionCable.Subscription exposing (..)


type alias Subscription =
    SubscriptionStatus


newSubscription : Subscription
newSubscription =
    SubscriptionAttempted


type SubscriptionStatus
    = SubscriptionAttempted
    | Subscribed
    | SubscriptionRejected
    | Unsubscribed


isActive : Subscription -> Bool
isActive subscription =
    case subscription of
        SubscriptionAttempted ->
            True

        Subscribed ->
            True

        _ ->
            False
