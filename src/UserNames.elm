module UserNames exposing (Store, UserId, UserNames, loading, setUserName, userNamesDecoder, usersCollectionPath)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Remote exposing (Remote(..))
import Store


type alias Store =
    Remote UserNames


loading : Store
loading =
    Loading


type alias UserId =
    String


type alias UserNames =
    Dict UserId String


setUserName : Store.Workspace -> UserId -> String -> Cmd msg
setUserName workspace userId userName =
    Store.setDoc
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
            (Store.dataField "name" Decode.string)
        )
        |> Decode.map Dict.fromList


usersCollectionPath : Store.Workspace -> Store.Path
usersCollectionPath workspace =
    [ "users" ]
        |> Store.prependWorkspace workspace
