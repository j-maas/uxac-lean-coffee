port module Store exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Remote exposing (Remote(..))


type Store
    = Store
        { userNames : Remote UserNames
        , speakers : Speakers
        }


type alias UserId =
    String



-- UserNames


type alias UserNames =
    Dict UserId String


getUserNames : Store -> Remote UserNames
getUserNames (Store store) =
    store.userNames


setUserName : UserId -> String -> Cmd msg
setUserName userId userName =
    setDoc
        { docPath = usersCollectionPath ++ [ userId ]
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


type alias Speakers =
    List UserId


getSpeakers : Store -> Speakers
getSpeakers (Store store) =
    store.speakers


enqueue : UserId -> Store -> Store
enqueue userId (Store store) =
    Store { store | speakers = store.speakers ++ [ userId ] }



-- Functions


init : Store
init =
    Store
        { userNames = Loading
        , speakers = [ "Johannes", "Roksy", "Jonathan" ]
        }


type Msg
    = UsersReceived (List UserNameEntry)


update : Msg -> Store -> Store
update msg (Store store) =
    case msg of
        UsersReceived userList ->
            let
                users =
                    Dict.fromList userList
            in
            Store { store | userNames = Got users }


dataField : String -> Decoder a -> Decoder a
dataField field decoder =
    Decode.field "data" (Decode.field field decoder)



-- Ports


usersCollectionPath : Path
usersCollectionPath =
    [ "users" ]


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
