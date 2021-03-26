module SortedDictTest exposing (..)

import Expect 
import Fuzz
import SortedDict exposing (SortedDict)
import Test exposing (..)


suite : Test
suite =
    describe "SortedDict"
        [ fuzz (Fuzz.list Fuzz.int) "keeps insertion order" <|
            \list ->
                let
                    ( withIds, sortedDict ) =
                        fromList list
                in
                SortedDict.toList sortedDict
                    |> Expect.equal withIds
        , fuzz (Fuzz.list Fuzz.int) "sorts items" <|
            \list ->
                let
                    ( _, sortedDict ) =
                        fromList list
                in
                sortedDict
                    |> SortedDict.stableSortWith (\( _, item1 ) ( _, item2 ) -> compare item1 item2)
                    |> SortedDict.toList
                    |> List.map (\( _, item ) -> item)
                    |> Expect.equal (List.sort list)
        ]


fromList : List Int -> ( List ( Int, Int ), SortedDict Int Int )
fromList list =
    let
        withIds =
            List.indexedMap (\id item -> ( id, item )) list

        sortedDict =
            List.foldl
                (\( id, item ) sd ->
                    SortedDict.insert id item sd
                )
                SortedDict.empty
                withIds
    in
    ( withIds, sortedDict )
