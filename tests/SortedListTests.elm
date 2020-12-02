module SortedListTests exposing (..)

import Expect
import Fuzz
import SortedList
import Test exposing (..)


suite : Test
suite =
    describe "SortedList"
        [ fuzz (Fuzz.list Fuzz.int) "sorts immediately" <|
            \list ->
                SortedList.from compare list
                    |> SortedList.toList
                    |> Expect.equalLists (List.sort list)

        {- We can't fuzz this, because we run into a "maximum call stack size exceeded".
           See https://github.com/elm-explorations/test/issues/25
        -}
        , test "adds new entries to the end" <|
            \_ ->
                let
                    firstList =
                        [ 4, 1, 3 ]

                    secondList =
                        [ 6, 2 ]
                in
                SortedList.from compare firstList
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
                SortedList.from compare initialList
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
                SortedList.from compare initialList
                    |> SortedList.update compare identity updatedList
                    |> SortedList.toList
                    |> Expect.equalLists [ 1, 3, 4, 2 ]
        ]
