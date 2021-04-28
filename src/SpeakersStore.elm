module SpeakersStore exposing (ContributionId, Speaker, SpeakerEntry, SpeakerList, SpeakersStore, fromList, speakers)

import User exposing (UserId)


type SpeakersStore
    = SpeakersStore SpeakerList


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


fromList : SpeakerList -> SpeakersStore
fromList list =
    SpeakersStore list


type alias Speakers =
    { activeSpeaker : SpeakerEntry
    , followUpSpeakers : SpeakerList
    }


speakers : SpeakersStore -> Maybe Speakers
speakers (SpeakersStore list) =
    case list of
        active :: followUps ->
            Just
                { activeSpeaker = active
                , followUpSpeakers = followUps
                }

        [] ->
            Nothing
