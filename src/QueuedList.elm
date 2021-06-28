module QueuedList exposing (QueuedList, allToList, filterMap, map, isEmpty)


type alias QueuedList a =
    { active : List a
    , queueing : List a
    }


isEmpty : QueuedList a -> Bool
isEmpty list =
    List.isEmpty list.active && List.isEmpty list.queueing


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


allToList : QueuedList a -> List a
allToList list =
    list.active ++ list.queueing
