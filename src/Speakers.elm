module Speakers exposing (ContributionId, Speaker, Speakers, followUpSpeakers, fromList)

import User exposing (UserId)


type Speakers
    = Speakers SpeakerList


type alias SpeakerList =
    List ( ContributionId, Speaker )


type alias ContributionId =
    String


type alias Speaker =
    { userId : UserId
    , name : String
    }


fromList : SpeakerList -> Speakers
fromList list =
    Speakers list


followUpSpeakers : Speakers -> SpeakerList
followUpSpeakers (Speakers list) =
    list
