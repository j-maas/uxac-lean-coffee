module SortedDict exposing (SortedDict, empty, insert, remove, stableSortWith, toList)

import Dict exposing (Dict)
import Time


type SortedDict comparable a
    = SortedDict
        { order : List comparable
        , items : Dict comparable a
        }


empty : SortedDict comparable a
empty =
    SortedDict { order = [], items = Dict.empty }


insert : comparable -> a -> SortedDict comparable a -> SortedDict comparable a
insert id item (SortedDict old) =
    let
        newOrder =
            removeFromOrder id old.order
                ++ [ id ]
    in
    SortedDict { order = newOrder, items = Dict.insert id item old.items }


remove : comparable -> SortedDict comparable a -> SortedDict comparable a
remove id (SortedDict old) =
    SortedDict
        { order = removeFromOrder id old.order
        , items = Dict.remove id old.items
        }


removeFromOrder : comparable -> List comparable -> List comparable
removeFromOrder id old =
    List.filter (\candidateId -> candidateId /= id) old


stableSortWith : (( comparable, a ) -> ( comparable, a ) -> Order) -> SortedDict comparable a -> SortedDict comparable a
stableSortWith sorting (SortedDict old) =
    SortedDict
        { order =
            toList (SortedDict old)
                -- The default sort appears to be stable.
                |> List.sortWith sorting
                |> List.map (\( id, _ ) -> id)
        , items = old.items
        }


toList : SortedDict comparable a -> List ( comparable, a )
toList (SortedDict sortedDict) =
    List.filterMap
        (\id ->
            case Dict.get id sortedDict.items of
                Just item ->
                    Just ( id, item )

                Nothing ->
                    Nothing
        )
        sortedDict.order
