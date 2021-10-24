module UserNames exposing (Store, UserId, UserNameEntry(..), UserNames, extractName, get, loading, setUserName, userNamesDecoder, usersCollectionPath)

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


type UserNames
    = UserNames (Dict UserId String)


fromList : List ( UserId, String ) -> UserNames
fromList list =
    UserNames (Dict.fromList list)


type UserNameEntry
    = UniqueName String
    | NameCollision String (List UserId)
    | MissingName


extractName : UserNameEntry -> Maybe String
extractName entry =
    case entry of
        UniqueName name ->
            Just name

        NameCollision name _ ->
            Just name

        MissingName ->
            Nothing


get : UserId -> UserNames -> UserNameEntry
get userId (UserNames dict) =
    case Dict.get userId dict of
        Just name ->
            let
                collisions =
                    Dict.toList dict |> List.filter (\( _, otherName ) -> otherName == name)
            in
            if List.length collisions > 1 then
                NameCollision name (List.map (\( id, _ ) -> id) collisions)

            else
                UniqueName name

        Nothing ->
            MissingName


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
        |> Decode.map fromList


usersCollectionPath : Store.Workspace -> Store.Path
usersCollectionPath workspace =
    [ "users" ]
        |> Store.prependWorkspace workspace
