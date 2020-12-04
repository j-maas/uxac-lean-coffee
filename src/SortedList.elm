module SortedList exposing (SortedList, from, isSorted, sort, toList, update)


type SortedList a
    = SortedList (List a)


type alias Ordering a =
    a -> a -> Order


type alias Id a comparable =
    a -> comparable


from : List a -> SortedList a
from list =
    SortedList list


update : Ordering a -> Id a comparable -> List a -> SortedList a -> SortedList a
update ord id newList (SortedList currentList) =
    let
        newSorted =
            List.sortWith ord newList

        oldItems =
            List.filter (\item -> List.any (\candidate -> id candidate == id item) newList) currentList

        newItems =
            List.filter (\item -> not (List.any (\candidate -> id candidate == id item) currentList)) newList
                |> List.sortWith ord
    in
    SortedList (oldItems ++ newItems)


sort : Ordering a -> SortedList a -> SortedList a
sort ord (SortedList list) =
    let
        -- The sorting may have become stale, so we need to sort again.
        -- TODO: Investigate whether another design can prevent this.
        sortedList =
            List.sortWith ord list
    in
    SortedList sortedList


toList : SortedList a -> List a
toList (SortedList list) =
    list


isSorted : Ordering a -> SortedList a -> Bool
isSorted ord (SortedList list) =
    let
        sorted =
            List.sortWith ord list
    in
    sorted == list
