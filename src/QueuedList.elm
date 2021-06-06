module QueuedList exposing (..)


type alias QueuedList a =
    { active : List a
    , queueing : List a
    }


map : (a -> b) -> QueuedList a -> QueuedList b
map mapping list =
    { active = List.map mapping list.active
    , queueing = List.map mapping list.queueing
    }


filterMap : (a -> Maybe b) -> QueuedList a -> QueuedList b
filterMap mapping list =
    { active = List.filterMap mapping list.active
    , queueing = List.filterMap mapping list.queueing
    }