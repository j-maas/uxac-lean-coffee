module SortedListTests exposing (..)

import Expect
import Fuzz
import SortedList
import Test exposing (..)


suite : Test
suite =
    {- We can't fuzz these, because we run into a "maximum call stack size exceeded".
       See https://github.com/elm-explorations/test/issues/25
    -}
    describe "SortedList"
        [ test "does not sort immediately" <|
            \_ ->
                let
                    list =
                        [ 3, 1, 2 ]
                in
                SortedList.from list
                    |> SortedList.toList
                    |> Expect.equalLists list
        , test "adds new entries to the end" <|
            \_ ->
                let
                    firstList =
                        [ 4, 1, 3 ]

                    secondList =
                        [ 6, 2 ]
                in
                SortedList.from firstList
                    |> SortedList.sort compare
                    |> SortedList.update compare identity (firstList ++ secondList)
                    |> SortedList.toList
                    |> Expect.equalLists (List.sort firstList ++ List.sort secondList)
        , test "removes items" <|
            \_ ->
                let
                    initialList =
                        [ 4, 1, 3 ]

                    updatedList =
                        [ 2, 1 ]
                in
                SortedList.from initialList
                    |> SortedList.update compare identity updatedList
                    |> SortedList.toList
                    |> Expect.equalLists (List.sort updatedList)
        , test "merges new list, removing elements while adding new ones to the end" <|
            \_ ->
                let
                    initialList =
                        [ 4, 1, 3 ]

                    updatedList =
                        [ 2, 4, 3, 1 ]
                in
                SortedList.from initialList
                    |> SortedList.sort compare
                    |> SortedList.update compare identity updatedList
                    |> SortedList.toList
                    |> Expect.equalLists [ 1, 3, 4, 2 ]
        ]
