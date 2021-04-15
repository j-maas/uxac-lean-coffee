module Store exposing (..)

import Dict
import Json.Decode as Decode exposing (Decoder)
import Remote exposing (Remote(..))


type Store
    = Store
        { users : Remote (List User)
        }


type alias UserId =
    String


type alias User =
    { id : UserId, name : String }


init : Store
init =
    Store
        { users = Loading
        }


type Msg
    = UsersReceived (List User)


update : Msg -> Store -> Store
update msg (Store store) =
    case msg of
        UsersReceived users ->
            Store { store | users = Got users }


usersDecoder : Decoder (List User)
usersDecoder =
    Decode.list
        (Decode.map2
            (\userId name ->
                { id = userId
                , name = name
                }
            )
            (dataField "userId" Decode.string)
            (dataField "name" Decode.string)
        )


dataField : String -> Decoder a -> Decoder a
dataField field decoder =
    Decode.field "data" (Decode.field field decoder)
