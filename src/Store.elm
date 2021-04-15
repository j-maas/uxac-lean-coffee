module Store exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Remote exposing (Remote(..))


type Store
    = Store
        { userNames : Remote UserNames
        }


type alias UserId =
    String


type alias UserNames =
    Dict UserId String


type alias UserEntry =
    ( UserId
    , String
    )


getUserNames : Store -> Remote UserNames
getUserNames (Store store) =
    store.userNames


init : Store
init =
    Store
        { userNames = Loading
        }


type Msg
    = UsersReceived (List UserEntry)


update : Msg -> Store -> Store
update msg (Store store) =
    case msg of
        UsersReceived userList ->
            let
                users =
                    Dict.fromList userList
            in
            Store { store | userNames = Got users }


userNamesDecoder : Decoder (List UserEntry)
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


dataField : String -> Decoder a -> Decoder a
dataField field decoder =
    Decode.field "data" (Decode.field field decoder)
