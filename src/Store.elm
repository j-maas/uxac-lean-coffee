port module Store exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Remote exposing (Remote(..))
import SpeakersStore exposing (ContributionId, SpeakersStore)
import Time
import User exposing (UserId)


type Store
    = Store
        { userNames : Remote UserNames
        , speakers : Remote DecodedSpeakerList
        }



-- UserNames


type alias UserNames =
    Dict UserId String


getUserNames : Store -> Remote UserNames
getUserNames (Store store) =
    store.userNames


setUserName : Workspace -> UserId -> String -> Cmd msg
setUserName workspace userId userName =
    setDoc
        { docPath = usersCollectionPath workspace ++ [ userId ]
        , doc = Encode.object [ ( "name", Encode.string userName ) ]
        }


userNamesDecoder : Decoder UserNames
userNamesDecoder =
    Decode.list
        (Decode.map2
            (\userId name ->
                ( userId
                , name
                )
            )
            (Decode.field "id" Decode.string)
            (dataField "name" Decode.string)
        )
        |> Decode.map Dict.fromList



-- Speakers


getSpeakers : Store -> Remote SpeakersStore
getSpeakers (Store store) =
    case ( store.userNames, store.speakers ) of
        ( Got userNames, Got speakers ) ->
            Got
                (speakers
                    -- TODO: Do not ignore nameless speakers, show them to the user.
                    |> List.filterMap
                        (\( contributionId, userId ) ->
                            Dict.get userId userNames
                                |> Maybe.map
                                    (\name ->
                                        ( contributionId, { userId = userId, name = name } )
                                    )
                        )
                    |> SpeakersStore.fromList
                )

        _ ->
            Loading


enqueue : Workspace -> TimestampField -> UserId -> Cmd msg
enqueue workspace timestamp userId =
    insertDoc
        { collectionPath = speakersCollectionPath workspace
        , doc =
            Encode.object
                [ ( "userId", Encode.string userId )
                , ( "whenEnqueued", timestamp )
                ]
        }


removeSpeakerContribution : Workspace -> ContributionId -> Cmd msg
removeSpeakerContribution workspace speakerContributionId =
    deleteDocs
        [ speakersCollectionPath workspace ++ [ speakerContributionId ]
        ]


type alias DecodedSpeakerList =
    List ( ContributionId, UserId )


speakersDecoder : Decoder DecodedSpeakerList
speakersDecoder =
    Decode.list
        (Decode.map3
            (\speakerContributionId userId maybeEnqueued ->
                ( maybeEnqueued
                , ( speakerContributionId, userId )
                )
            )
            (Decode.field "id" Decode.string)
            (dataField "userId" Decode.string)
            (dataField "whenEnqueued" (Decode.maybe timestampDecoder))
        )
        {- Firestore updates the collection locally, but the timestamp is null until the server responds.
           We ignore such entries until they are available with a timestamp.
        -}
        |> Decode.map
            (List.filterMap
                (\( maybeEnqueued, speaker ) ->
                    Maybe.map (\enqueued -> ( enqueued, speaker )) maybeEnqueued
                )
            )
        |> Decode.map (List.sortBy (\( enqueued, _ ) -> Time.posixToMillis enqueued))
        |> Decode.map (List.map Tuple.second)



-- Functions


init : Store
init =
    Store
        { userNames = Loading
        , speakers = Loading
        }


type Msg
    = UsersReceived UserNames
    | SpeakersReceived DecodedSpeakerList


update : Msg -> Store -> Store
update msg (Store store) =
    case msg of
        UsersReceived users ->
            Store { store | userNames = Got users }

        SpeakersReceived decodedSpeakers ->
            Store { store | speakers = Got decodedSpeakers }


dataField : String -> Decoder a -> Decoder a
dataField field decoder =
    Decode.field "data" (Decode.field field decoder)



-- Ports


type alias TimestampField =
    Decode.Value


timestampDecoder : Decoder Time.Posix
timestampDecoder =
    Decode.map2
        (\seconds nanoseconds ->
            let
                {- The nanoseconds count the fractions of seconds.
                   See https://firebase.google.com/docs/reference/js/firebase.firestore.Timestamp.
                -}
                milliseconds =
                    (toFloat seconds * 1000)
                        + (toFloat nanoseconds / 1000000)
            in
            Time.millisToPosix (round milliseconds)
        )
        (Decode.field "seconds" Decode.int)
        (Decode.field "nanoseconds" Decode.int)


usersCollectionPath : Workspace -> Path
usersCollectionPath workspace =
    [ "users" ]
        |> prependWorkspace workspace


speakersCollectionPath : Workspace -> Path
speakersCollectionPath workspace =
    [ "speakers" ]
        |> prependWorkspace workspace


type alias Workspace =
    String


prependWorkspace : Workspace -> Path -> Path
prependWorkspace workspace path =
    let
        serializedWorkspace =
            "_" ++ workspace
    in
    [ "workspaces", serializedWorkspace ] ++ path


type alias Path =
    List String


encodePath : Path -> Encode.Value
encodePath path =
    String.join "/" path
        |> Encode.string


type alias SetDocInfo =
    { docPath : Path, doc : Encode.Value }


setDoc : SetDocInfo -> Cmd msg
setDoc info =
    Encode.object
        [ ( "path", encodePath info.docPath )
        , ( "doc", info.doc )
        ]
        |> setDoc_


port setDoc_ : Encode.Value -> Cmd msg


type alias InsertDocInfo =
    { collectionPath : Path, doc : Encode.Value }


insertDoc : InsertDocInfo -> Cmd msg
insertDoc info =
    Encode.object
        [ ( "path", encodePath info.collectionPath )
        , ( "doc", info.doc )
        ]
        |> insertDoc_


port insertDoc_ : Encode.Value -> Cmd msg


deleteDocs : List Path -> Cmd msg
deleteDocs paths =
    Encode.object
        [ ( "paths", Encode.list encodePath paths )
        ]
        |> deleteDocs_


port deleteDocs_ : Encode.Value -> Cmd msg
