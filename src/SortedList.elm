module SortedList exposing (SortedList, current, from)


type SortedList a
    = Sorted (List a)
    | Unsorted { curr : List a, sorted : List a }


from : List a -> SortedList a
from list =
    Sorted list


current : SortedList a -> List a
current sortedList =
    case sortedList of
        Sorted list ->
            list

        Unsorted { curr } ->
            curr
