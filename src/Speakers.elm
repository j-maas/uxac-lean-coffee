module Speakers exposing (ContributionId, CurrentSpeaker, DecodedSpeakers, Speaker, SpeakerEntry, SpeakerList, SpeakerName(..), Speakers, SpeakersQueue, Store, ask, clearAll, displayName, enqueue, get, loading, questionCollectionPath, removeQuestion, removeSpeakerContribution, speakerCollectionPath, speakersDecoder, updateQuestions, updateSpeakers, editReminder)

import HumanReadableId
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import QueuedList exposing (QueuedList)
import Remote exposing (Remote(..))
import Store
import Time
import UserNames exposing (UserId, UserNameEntry(..))


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
    ( ContributionId, Speaker, String )


type alias ContributionId =
    String


type alias Speaker =
    { userId : UserId
    , name : UserNameEntry
    }


type SpeakerName
    = CustomName String
    | GeneratedName String


displayName : Speaker -> SpeakerName
displayName speaker =
    case speaker.name of
        UniqueName name ->
            CustomName name

        NameCollision name _ ->
            CustomName name

        MissingName ->
            GeneratedName (HumanReadableId.humanize speaker.userId)


enqueue : Store.Workspace -> Store.TimestampField -> UserId -> String -> Cmd msg
enqueue workspace timestamp userId reminder =
    Store.insertDoc
        { collectionPath = speakerCollectionPath workspace
        , doc = speakerEncoder timestamp userId reminder
        }


editReminder : Store.Workspace -> ContributionId -> String -> Cmd msg
editReminder workspace contributionId newReminder =
    Store.editDoc
        { docPath = speakerCollectionPath workspace ++ [ contributionId ]
        , fields = [ ( "reminder", Encode.string newReminder ) ]
        }


removeSpeakerContribution : Store.Workspace -> ContributionId -> Cmd msg
removeSpeakerContribution workspace speakerContributionId =
    Store.deleteDocs
        [ speakerCollectionPath workspace ++ [ speakerContributionId ]
        ]


ask : Store.Workspace -> Store.TimestampField -> UserId -> String -> Cmd msg
ask workspace timestamp userId reminder =
    Store.insertDoc
        { collectionPath = questionCollectionPath workspace
        , doc = speakerEncoder timestamp userId reminder
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
            (\( contributionId, userId, reminder ) ->
                ( contributionId
                , { userId = userId
                  , name =
                        UserNames.get userId userNames
                  }
                , reminder
                )
            )


clearAll : Store.Workspace -> Store -> Cmd msg
clearAll workspace (Store store) =
    case ( store.speakers, store.questions ) of
        ( Got speakers, Got questions ) ->
            let
                speakerDocs =
                    List.map (\( id, _, _ ) -> speakerCollectionPath workspace ++ [ id ]) (QueuedList.allToList speakers)

                questionDocs =
                    List.map (\( id, _, _ ) -> questionCollectionPath workspace ++ [ id ]) (QueuedList.allToList questions)
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
    ( ContributionId, UserId, String )


speakerEncoder : Store.TimestampField -> UserId -> String -> Encode.Value
speakerEncoder timestamp userId reminder =
    Encode.object
        ([ ( "userId", Encode.string userId )
         , ( "createdAt", timestamp )
         ]
            ++ (if String.trim reminder |> String.isEmpty then
                    []

                else
                    [ ( "reminder", Encode.string reminder ) ]
               )
        )


speakersDecoder : Decoder DecodedSpeakers
speakersDecoder =
    Decode.list
        (Decode.map4
            (\speakerContributionId userId maybeReminder maybeCreatedAt ->
                ( maybeCreatedAt
                , ( speakerContributionId, userId, maybeReminder |> Maybe.withDefault "" )
                )
            )
            (Decode.field "id" Decode.string)
            (Store.dataField "userId" Decode.string)
            (Decode.maybe <| Store.dataField "reminder" Decode.string)
            (Store.dataField "createdAt" (Decode.maybe Store.timestampDecoder))
        )
        {- Firestore updates the collection locally, but the timestamp is null until the server responds.
           We treat such entries as queued until they are available with a timestamp.
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
