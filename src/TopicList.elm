module TopicList exposing (..)

import SortedList exposing (SortedList)
import Time


type alias Topic =
    { id : TopicId
    , topic : String
    , userId : String
    , createdAt : Maybe Time.Posix
    }


type alias TopicId =
    String


type alias TopicList =
    SortedList Topic


type alias VoteCountMap =
    Topic -> Int


from : VoteCountMap -> List Topic -> TopicList
from voteMap topics =
    SortedList.from (orderTopics voteMap) topics


update : VoteCountMap -> List Topic -> TopicList -> TopicList
update voteMap newTopics topicList =
    SortedList.update (orderTopics voteMap) .id newTopics topicList


sort : VoteCountMap -> TopicList -> TopicList
sort voteMap topicList =
    SortedList.sort (orderTopics voteMap) topicList


toList : TopicList -> List Topic
toList topicList =
    SortedList.toList topicList


isSorted : VoteCountMap -> TopicList -> Bool
isSorted voteMap topicList =
    SortedList.isSorted (orderTopics voteMap) topicList


orderTopics : VoteCountMap -> Topic -> Topic -> Order
orderTopics voteMap first second =
    let
        ( firstVotes, secondVotes ) =
            ( voteMap first
            , voteMap second
            )
    in
    case compare firstVotes secondVotes of
        EQ ->
            case ( first.createdAt, second.createdAt ) of
                ( Just firstTS, Just secondTS ) ->
                    compare
                        (Time.posixToMillis firstTS)
                        (Time.posixToMillis secondTS)

                {- A missing timestamp means that the item was added locally.
                   Then it should be sorted at the end of the, i.e., larger.
                -}
                ( Nothing, Just _ ) ->
                    GT

                ( Just _, Nothing ) ->
                    LT

                ( Nothing, Nothing ) ->
                    EQ

        -- Invert the order, so that the most votes are first.
        GT ->
            LT

        LT ->
            GT
