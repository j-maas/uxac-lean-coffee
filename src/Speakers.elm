module Speakers exposing (ContributionId, CurrentSpeaker, DecodedSpeakers, Speaker, SpeakerEntry, SpeakerList, Speakers, Store, ask, enqueue, get, loading, questionCollectionPath, receivedQuestions, receivedSpeakers, removeQuestion, removeSpeakerContribution, speakerCollectionPath, speakersDecoder)

import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import QueuedList exposing (QueuedList)
import Remote exposing (Remote(..))
import Store
import Time
import UserNames exposing (UserId)


type alias Speakers =
    { current : CurrentSpeaker
    , following : SpeakerList
    }


type alias CurrentSpeaker =
    { speaker : SpeakerEntry
    , questions : SpeakerList
    }


type alias SpeakerList =
    QueuedList SpeakerEntry


type alias SpeakerEntry =
    ( ContributionId, Speaker )


type alias ContributionId =
    String


type alias Speaker =
    { userId : UserId
    , name : String
    }


enqueue : Store.Workspace -> Store.TimestampField -> UserId -> Cmd msg
enqueue workspace timestamp userId =
    Store.insertDoc
        { collectionPath = speakerCollectionPath workspace
        , doc = speakerEncoder timestamp userId
        }


removeSpeakerContribution : Store.Workspace -> ContributionId -> Cmd msg
removeSpeakerContribution workspace speakerContributionId =
    Store.deleteDocs
        [ speakerCollectionPath workspace ++ [ speakerContributionId ]
        ]


ask : Store.Workspace -> Store.TimestampField -> UserId -> Cmd msg
ask workspace timestamp userId =
    Store.insertDoc
        { collectionPath = questionCollectionPath workspace
        , doc = speakerEncoder timestamp userId
        }


removeQuestion : Store.Workspace -> ContributionId -> Cmd msg
removeQuestion workspace speakerContributionId =
    Store.deleteDocs
        [ questionCollectionPath workspace ++ [ speakerContributionId ]
        ]



-- BACKEND


type Store
    = Store
        { speakers : Remote DecodedSpeakers
        , questions : Remote DecodedSpeakers
        }


loading : Store
loading =
    Store
        { speakers = Loading
        , questions = Loading
        }


get : UserNames.Store -> Store -> Remote (Maybe Speakers)
get userStore (Store store) =
    Got
        (\userNames rawSpeakers rawQuestions ->
            let
                speakers =
                    mapToSpeakerList userNames rawSpeakers

                questions =
                    mapToSpeakerList userNames rawQuestions
            in
            case speakers.active of
                current :: following ->
                    Just
                        { current =
                            { speaker = current
                            , questions = questions
                            }
                        , following =
                            { active = following
                            , queueing = speakers.queueing
                            }
                        }

                [] ->
                    Nothing
        )
        |> Remote.andMap userStore
        |> Remote.andMap store.speakers
        |> Remote.andMap store.questions


mapToSpeakerList : UserNames.UserNames -> DecodedSpeakers -> SpeakerList
mapToSpeakerList userNames rawSpeakers =
    rawSpeakers
        -- TODO: Do not ignore nameless speakers, show them to the user.
        |> QueuedList.filterMap
            (\( contributionId, userId ) ->
                Dict.get userId userNames
                    |> Maybe.map
                        (\name ->
                            ( contributionId, { userId = userId, name = name } )
                        )
            )


receivedSpeakers : DecodedSpeakers -> Store -> Store
receivedSpeakers newSpeakers (Store store) =
    Store
        { store
            | speakers = Got newSpeakers
        }


receivedQuestions : DecodedSpeakers -> Store -> Store
receivedQuestions newQuestions (Store store) =
    Store
        { store
            | questions = Got newQuestions
        }


type alias DecodedSpeakers =
    QueuedList DecodedSpeakerEntry


type alias DecodedSpeakerEntry =
    ( ContributionId, UserId )


speakerEncoder : Store.TimestampField -> UserId -> Encode.Value
speakerEncoder timestamp userId =
    Encode.object
        [ ( "userId", Encode.string userId )
        , ( "createdAt", timestamp )
        ]


speakersDecoder : Decoder DecodedSpeakers
speakersDecoder =
    Decode.list
        (Decode.map3
            (\speakerContributionId userId maybeCreatedAt ->
                ( maybeCreatedAt
                , ( speakerContributionId, userId )
                )
            )
            (Decode.field "id" Decode.string)
            (Store.dataField "userId" Decode.string)
            (Store.dataField "createdAt" (Decode.maybe Store.timestampDecoder))
        )
        {- Firestore updates the collection locally, but the timestamp is null until the server responds.
           We ignore such entries until they are available with a timestamp.
        -}
        |> Decode.map
            (\list ->
                let
                    ( unsortedSpeakers, queueing ) =
                        List.foldl
                            (\( maybeCreatedAt, speaker ) ( currentUnsortedSpeakers, currentQueueing ) ->
                                case maybeCreatedAt of
                                    Just createdAt ->
                                        ( currentUnsortedSpeakers ++ [ ( createdAt, speaker ) ], currentQueueing )

                                    Nothing ->
                                        ( currentUnsortedSpeakers, currentQueueing ++ [ speaker ] )
                            )
                            ( [], [] )
                            list

                    speakers =
                        unsortedSpeakers
                            |> List.sortBy (\( enqueued, _ ) -> Time.posixToMillis enqueued)
                            |> List.map Tuple.second
                in
                { active = speakers
                , queueing = queueing
                }
            )


speakerCollectionPath : Store.Workspace -> Store.Path
speakerCollectionPath workspace =
    [ "speakers" ]
        |> Store.prependWorkspace workspace


questionCollectionPath : Store.Workspace -> Store.Path
questionCollectionPath workspace =
    [ "questions" ]
        |> Store.prependWorkspace workspace
