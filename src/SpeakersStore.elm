module SpeakersStore exposing (ActiveSpeaker, ContributionId, Speaker, SpeakerEntry, SpeakerList, SpeakersStore, init, speakers)

import User exposing (UserId)


type SpeakersStore
    = SpeakersStore
        { speakers : SpeakerList
        , questions : SpeakerList
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


init : { speakers : SpeakerList, questions : SpeakerList } -> SpeakersStore
init lists =
    SpeakersStore lists


type alias Speakers =
    { activeSpeaker : ActiveSpeaker
    , followUpSpeakers : SpeakerList
    }


type alias ActiveSpeaker =
    { speaker : SpeakerEntry
    , questions : SpeakerList
    }


speakers : SpeakersStore -> Maybe Speakers
speakers (SpeakersStore list) =
    case list.speakers of
        active :: followUps ->
            Just
                { activeSpeaker =
                    { speaker = active
                    , questions = list.questions
                    }
                , followUpSpeakers = followUps
                }

        [] ->
            Nothing
