port module Store exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Remote exposing (Remote(..))
import Time


dataField : String -> Decoder a -> Decoder a
dataField field decoder =
    Decode.field "data" (Decode.field field decoder)


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
