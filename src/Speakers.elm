module Speakers exposing (ContributionId, CurrentSpeaker, DecodedSpeakers, Speaker, SpeakerEntry, SpeakerList, Speakers, SpeakersQueue, Store, ask, clearAll, enqueue, get, loading, questionCollectionPath, removeQuestion, removeSpeakerContribution, speakerCollectionPath, speakersDecoder, updateQuestions, updateSpeakers, displayName)

import Dict
import HumanReadableId
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import QueuedList exposing (QueuedList)
import Remote exposing (Remote(..))
import Store
import Time
import UserNames exposing (UserId)


type alias SpeakersQueue =
    { speakers : Maybe Speakers
    , queueing : Bool
    }


type alias Speakers =
    { current : CurrentSpeaker
    , following : SpeakerList
    }


type alias CurrentSpeaker =
    { speaker : SpeakerEntry
    , questions : SpeakerList
    , questionsQueueing : Bool
    }


type alias SpeakerList =
    List SpeakerEntry


type alias SpeakerEntry =
    ( ContributionId, Speaker )


type alias ContributionId =
    String


type alias Speaker =
    { userId : UserId
    , name : Maybe String
    }


displayName : Speaker -> String
displayName speaker =
    case speaker.name of
        Just name ->
            name

        Nothing ->
            HumanReadableId.humanize speaker.userId


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


get : UserNames.Store -> Store -> Remote SpeakersQueue
get userStore (Store store) =
    Got
        (\userNames rawSpeakers rawQuestions ->
            let
                speakers =
                    mapToSpeakerList userNames rawSpeakers

                questions =
                    mapToSpeakerList userNames rawQuestions
            in
            { speakers =
                case speakers.active of
                    current :: following ->
                        Just
                            { current =
                                { speaker = current
                                , questions = questions.active
                                , questionsQueueing = not (List.isEmpty questions.queueing)
                                }
                            , following = following
                            }

                    [] ->
                        Nothing
            , queueing = not (List.isEmpty speakers.queueing)
            }
        )
        |> Remote.andMap userStore
        |> Remote.andMap store.speakers
        |> Remote.andMap store.questions


mapToSpeakerList : UserNames.UserNames -> DecodedSpeakers -> QueuedList SpeakerEntry
mapToSpeakerList userNames rawSpeakers =
    rawSpeakers
        |> QueuedList.map
            (\( contributionId, userId ) ->
                ( contributionId
                , { userId = userId
                  , name =
                        Dict.get userId userNames
                  }
                )
            )


clearAll : Store.Workspace -> Store -> Cmd msg
clearAll workspace (Store store) =
    case ( store.speakers, store.questions ) of
        ( Got speakers, Got questions ) ->
            let
                speakerDocs =
                    List.map (\( id, _ ) -> speakerCollectionPath workspace ++ [ id ]) (QueuedList.allToList speakers)

                questionDocs =
                    List.map (\( id, _ ) -> questionCollectionPath workspace ++ [ id ]) (QueuedList.allToList questions)
            in
            Store.deleteDocs (speakerDocs ++ questionDocs)

        _ ->
            Cmd.none


updateSpeakers : DecodedSpeakers -> Store -> Store
updateSpeakers newSpeakers (Store store) =
    Store
        { store
            | speakers = Got newSpeakers
        }


updateQuestions : DecodedSpeakers -> Store -> Store
updateQuestions newQuestions (Store store) =
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
