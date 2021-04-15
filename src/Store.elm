module Store exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Remote exposing (Remote(..))


type Store
    = Store
        { users : Remote Users
        }


type alias UserId =
    String


type alias Users =
    Dict UserId { name : String }


type alias UserEntry =
    ( UserId
    , { name : String
      }
    )


getUsers : Store -> Remote Users
getUsers (Store store) =
    store.users


init : Store
init =
    Store
        { users = Loading
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
            Store { store | users = Got users }


usersDecoder : Decoder (List UserEntry)
usersDecoder =
    Decode.list
        (Decode.map2
            (\userId name ->
                ( userId
                , { name = name
                  }
                )
            )
            (Decode.field "id" Decode.string)
            (dataField "name" Decode.string)
        )


dataField : String -> Decoder a -> Decoder a
dataField field decoder =
    Decode.field "data" (Decode.field field decoder)
