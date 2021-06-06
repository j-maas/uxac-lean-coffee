module Speakers exposing (ActiveSpeaker, ContributionId, DecodedSpeakerList, Speaker, SpeakerEntry, SpeakerList, Speakers, Store, ask, enqueue, get, loading, questionCollectionPath, receivedQuestions, receivedSpeakers, removeQuestion, removeSpeakerContribution, speakerCollectionPath, speakersDecoder)

import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Remote exposing (Remote(..))
import Store
import Time
import UserNames exposing (UserId)


type Store
    = Store
        { speakers : Remote DecodedSpeakerList
        , questions : Remote DecodedSpeakerList
        }


loading : Store
loading =
    Store
        { speakers = Loading
        , questions = Loading
        }


receivedSpeakers : DecodedSpeakerList -> Store -> Store
receivedSpeakers newSpeakers (Store store) =
    Store
        { store
            | speakers = Got newSpeakers
        }


receivedQuestions : DecodedSpeakerList -> Store -> Store
receivedQuestions newQuestions (Store store) =
    Store
        { store
            | questions = Got newQuestions
        }


type alias SpeakerList =
    List SpeakerEntry


type alias SpeakerEntry =
    ( ContributionId, Speaker )


type alias ContributionId =
    String


type alias Speaker =
    { userId : UserId
    , name : String
    }


type alias Speakers =
    { activeSpeaker : ActiveSpeaker
    , followUpSpeakers : SpeakerList
    }


type alias ActiveSpeaker =
    { speaker : SpeakerEntry
    , questions : SpeakerList
    }


get : UserNames.Store -> Store -> Remote (Maybe Speakers)
get userStore (Store store) =
    case ( userStore, store.speakers, store.questions ) of
        ( Got userNames, Got rawSpeakers, Got rawQuestions ) ->
            Got
                (let
                    speakers =
                        mapToSpeakerList userNames rawSpeakers

                    questions =
                        mapToSpeakerList userNames rawQuestions
                 in
                 case speakers of
                    active :: followUps ->
                        Just
                            { activeSpeaker =
                                { speaker = active
                                , questions = questions
                                }
                            , followUpSpeakers = followUps
                            }

                    [] ->
                        Nothing
                )

        _ ->
            Loading


mapToSpeakerList : UserNames.UserNames -> DecodedSpeakerList -> SpeakerList
mapToSpeakerList userNames rawSpeakers =
    rawSpeakers
        -- TODO: Do not ignore nameless speakers, show them to the user.
        |> List.filterMap
            (\( contributionId, userId ) ->
                Dict.get userId userNames
                    |> Maybe.map
                        (\name ->
                            ( contributionId, { userId = userId, name = name } )
                        )
            )


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


type alias DecodedSpeakerList =
    List ( ContributionId, UserId )


speakerEncoder : Store.TimestampField -> UserId -> Encode.Value
speakerEncoder timestamp userId =
    Encode.object
        [ ( "userId", Encode.string userId )
        , ( "createdAt", timestamp )
        ]


speakersDecoder : Decoder DecodedSpeakerList
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
            (List.filterMap
                (\( maybeCreatedAt, speaker ) ->
                    Maybe.map (\enqueued -> ( enqueued, speaker )) maybeCreatedAt
                )
            )
        |> Decode.map (List.sortBy (\( enqueued, _ ) -> Time.posixToMillis enqueued))
        |> Decode.map (List.map Tuple.second)


speakerCollectionPath : Store.Workspace -> Store.Path
speakerCollectionPath workspace =
    [ "speakers" ]
        |> Store.prependWorkspace workspace


questionCollectionPath : Store.Workspace -> Store.Path
questionCollectionPath workspace =
    [ "questions" ]
        |> Store.prependWorkspace workspace
