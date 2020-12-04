port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Css exposing (auto, pct, px, rem, zero)
import Css.Global as Global
import Dict exposing (Dict)
import Html as PlainHtml
import Html.Styled as Html exposing (Html, button, div, form, h1, h2, input, label, li, ol, p, text, textarea)
import Html.Styled.Attributes exposing (css, placeholder, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline
import Json.Encode as Encode
import List.Extra as List
import Remote exposing (Remote(..))
import Time
import TopicList exposing (Topic, TopicId, TopicList, VoteCountMap)


main =
    Browser.element
        { view = view >> Html.toUnstyled
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL ----


type alias Model =
    { discussed : Maybe TopicId
    , topics : Remote TopicList
    , votes : Remote Votes
    , newTopicInput : String
    , user : Remote User
    , isAdmin : Bool
    , error : Maybe Error

    {- This allows us to specify that a server should add a timestamp
       in a document's field.
       See https://firebase.google.com/docs/firestore/manage-data/add-data#server_timestamp.
    -}
    , timestampField : TimestampField
    }


type alias Votes =
    Dict TopicId (List UserId)


type alias Vote =
    { userId : UserId, topicId : TopicId }


voteFrom : User -> Topic -> Vote
voteFrom user topic =
    { userId = user.id, topicId = topic.id }


type alias User =
    { id : UserId
    }


type alias UserId =
    String


type Error
    = FirestoreError { code : String, errorMessage : String }
    | ParsingError String


type alias TimestampField =
    Decode.Value


type alias Flags =
    { timestampField : TimestampField
    , isAdmin : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { discussed = Nothing
      , topics = Loading
      , votes = Loading
      , newTopicInput = ""
      , user = Loading
      , isAdmin = flags.isAdmin
      , error = Nothing
      , timestampField = flags.timestampField
      }
    , firestoreSubscriptionsCmd
    )



---- UPDATE ----


type Msg
    = UserReceived (Result Decode.Error User)
    | DecodeError Decode.Error
    | TopicsReceived (List Topic)
    | VotesReceived Votes
    | DiscussedTopicReceived (Maybe TopicId)
    | SaveTopic User
    | DeleteTopic TopicId
    | Discuss TopicId
    | SortTopics
    | Upvote User Topic
    | RemoveUpvote User Topic
    | ErrorReceived (Result Decode.Error Error)
    | NewTopicInputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        voteCountMap =
            voteCountMapFromVotes model.votes
    in
    case msg of
        UserReceived result ->
            case result of
                Ok user ->
                    ( { model | user = Got user }, Cmd.none )

                Err error ->
                    ( { model | error = Just (ParsingError (Decode.errorToString error)) }, Cmd.none )

        DecodeError error ->
            ( { model | error = Just <| ParsingError (Decode.errorToString error) }, Cmd.none )

        TopicsReceived topics ->
            ( { model | topics = updateTopicList voteCountMap topics model.topics }, Cmd.none )

        VotesReceived votes ->
            let
                newModel =
                    case model.votes of
                        Loading ->
                            { model
                                | votes = Got votes

                                -- If the votes are coming in for the first time, immediately sort the topics.
                                , topics =
                                    Remote.map
                                        (TopicList.sort <| voteCountMapFromVotes <| Got votes)
                                        model.topics
                            }

                        Got _ ->
                            { model | votes = Got votes }
            in
            ( newModel, Cmd.none )

        DiscussedTopicReceived topic ->
            ( { model | discussed = topic }, Cmd.none )

        ErrorReceived result ->
            case result of
                Ok value ->
                    ( { model | error = Just value }, Cmd.none )

                Err error ->
                    ( { model | error = Just (ParsingError (Decode.errorToString error)) }, Cmd.none )

        SaveTopic user ->
            ( { model | newTopicInput = "" }
            , submitTopic model.timestampField { topic = model.newTopicInput, userId = user.id }
            )

        DeleteTopic id ->
            ( model, deleteTopic id model.votes )

        Discuss topicId ->
            ( model, submitDiscussedTopic topicId )

        SortTopics ->
            ( { model | topics = Remote.map (TopicList.sort voteCountMap) model.topics }, Cmd.none )

        Upvote user topic ->
            ( model, submitVote (voteFrom user topic) )

        RemoveUpvote user topic ->
            ( model, retractVote (voteFrom user topic) )

        NewTopicInputChanged value ->
            ( { model | newTopicInput = value }, Cmd.none )


voteCountMapFromVotes : Remote Votes -> VoteCountMap
voteCountMapFromVotes remoteVotes topic =
    case remoteVotes of
        Loading ->
            0

        Got votes ->
            Dict.get topic.id votes |> Maybe.withDefault [] |> List.length


updateTopicList : VoteCountMap -> List Topic -> Remote TopicList -> Remote TopicList
updateTopicList voteCountMap newTopics currentTopics =
    (case currentTopics of
        Loading ->
            TopicList.from newTopics
                |> TopicList.sort voteCountMap

        Got sortedList ->
            TopicList.update voteCountMap newTopics sortedList
    )
        |> Got


submitTopic : TimestampField -> TopicSubmission -> Cmd msg
submitTopic timestampField submission =
    insertDoc
        { collectionPath = topicCollectionPath
        , data =
            topicEncoder
                submission
                timestampField
        }


deleteTopic : TopicId -> Remote Votes -> Cmd msg
deleteTopic topicId votes =
    let
        topicPath =
            topicCollectionPath ++ [ topicId ]

        {- We delete all associated votes as well.
           TODO: This smells like there might be a race condition when others add votes when we delete them. Investigate whether we can delete the associated votes on the server side through functions.
        -}
        votePaths =
            Remote.toMaybe votes
                |> Maybe.withDefault Dict.empty
                |> Dict.get topicId
                |> Maybe.withDefault []
                |> List.map (\userId -> votePath { userId = userId, topicId = topicId })
    in
    deleteDocs (topicPath :: votePaths)


submitVote : Vote -> Cmd msg
submitVote vote =
    setDoc
        { docPath = votePath vote
        , data =
            voteEncoder vote
        }


votePath : Vote -> Path
votePath ids =
    voteCollectionPath ++ [ ids.userId ++ ":" ++ ids.topicId ]


retractVote : Vote -> Cmd msg
retractVote vote =
    deleteDocs [ votePath vote ]


submitDiscussedTopic : TopicId -> Cmd msg
submitDiscussedTopic topicId =
    setDoc
        { docPath = discussedDocPath
        , data = discussedTopicEncoder topicId
        }



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        voteCountMap =
            voteCountMapFromVotes model.votes

        heading =
            "UXAC Lean Coffee"
                ++ (if model.isAdmin then
                        " (Admin)"

                    else
                        ""
                   )

        ( discussedTopic, topicList ) =
            processTopics model
    in
    div
        [ css
            [ bodyFont
            , Css.margin2 zero auto
            ]
        ]
        [ div
            [ css
                [ limitWidth
                , Css.marginBottom (rem 1)
                ]
            ]
            ([ h1 [] [ text heading ] ]
                ++ errorView model.error
                ++ discussionView model discussedTopic
                ++ [ div [ css [ Css.marginTop (rem 1) ] ] [ topicEntry model.user model.newTopicInput ] ]
            )
        , topicsToVote model topicList (sortBarView model.votes model.topics)
        ]


limitWidth : Css.Style
limitWidth =
    Css.batch
        [ Css.maxWidth (rem 32)
        , Css.marginLeft Css.auto
        , Css.marginRight Css.auto
        ]


type alias TopicWithVotes =
    { topic : Topic
    , votes : List UserId
    }


processTopics :
    { a
        | discussed : Maybe TopicId
        , topics : Remote TopicList
        , votes : Remote Votes
    }
    -> ( Maybe TopicWithVotes, Remote (List TopicWithVotes) )
processTopics model =
    case model.topics of
        Loading ->
            ( Nothing, Loading )

        Got topics ->
            let
                topicsInVoting =
                    Maybe.map
                        (\votes ->
                            TopicList.toList topics
                                |> List.map
                                    (\topic ->
                                        { topic = topic
                                        , votes =
                                            Dict.get topic.id votes
                                                |> Maybe.withDefault []
                                        }
                                    )
                        )
                        (Remote.toMaybe model.votes)
                        |> Maybe.withDefault []
            in
            model.discussed
                |> Maybe.map
                    (\topicId ->
                        extract (\entry -> entry.topic.id == topicId) topicsInVoting
                            |> Tuple.mapSecond Got
                    )
                |> Maybe.withDefault ( Nothing, Got topicsInVoting )


extract : (a -> Bool) -> List a -> ( Maybe a, List a )
extract predicate list =
    List.partition predicate list
        |> Tuple.mapFirst List.head


errorView : Maybe Error -> List (Html Msg)
errorView maybeError =
    case maybeError of
        Just error ->
            [ div
                [ css
                    [ Css.border3 (px 1) Css.solid (Css.hsl 0 0.8 0.75)
                    , Css.padding (rem 1)
                    ]
                ]
                [ text <| stringFromError error ]
            ]

        Nothing ->
            []


discussionView : TopicViewModel a -> Maybe TopicWithVotes -> List (Html Msg)
discussionView model maybeDiscussedTopic =
    case maybeDiscussedTopic of
        Just topic ->
            [ div
                [ css
                    [ Css.borderRadius (rem 0.5)
                    , Css.padding (rem 1)
                    , Css.backgroundColor (Css.hsl primaryHue 1 0.5)
                    ]
                ]
                [ h2 [ css [ Css.margin zero, Css.marginBottom (rem 1) ] ] [ text "In discussion" ]
                , topicCard model topic
                ]
            ]

        Nothing ->
            []


topicEntry : Remote User -> String -> Html Msg
topicEntry user newTopicInput =
    card [ submitForm user newTopicInput ]


topicsToVote : TopicViewModel a -> Remote (List TopicWithVotes) -> Html Msg -> Html Msg
topicsToVote model remoteTopics toolbar =
    div
        [ css
            [ Css.backgroundColor (Css.hsl primaryHue 0.2 0.95)
            , containerPadding
            , Css.borderRadius (rem 0.5)
            ]
        ]
        (case remoteTopics of
            Loading ->
                [ text "Loading topics…" ]

            Got topics ->
                [ div
                    [ css
                        [ Css.displayFlex
                        , Css.flexDirection Css.row
                        , Css.flexWrap Css.wrap
                        , Css.alignItems Css.center
                        , Css.marginBottom (rem 1)
                        ]
                    ]
                    [ h2
                        [ css
                            [ Css.margin zero
                            , Css.marginRight (rem 1)
                            ]
                        ]
                        [ text "All topics" ]
                    , toolbar
                    ]
                , ol
                    [ css
                        [ Css.property "display" "grid"

                        -- TODO: Consider allowing smaller widths on narrow devices via media queries.
                        , Css.property "grid-template-columns" "repeat(auto-fill, minmax(20rem, 1fr))"
                        , Css.property "row-gap" "2rem"
                        , Css.property "column-gap" "1rem"
                        , Css.padding zero
                        , Css.margin zero
                        ]
                    ]
                    (List.map
                        (\topic ->
                            li
                                [ css [ Css.listStyle Css.none ]
                                ]
                                [ topicCard model topic ]
                        )
                        topics
                    )
                ]
        )


containerPadding : Css.Style
containerPadding =
    Css.padding2 (rem 1) (rem 1)


bodyFont : Css.Style
bodyFont =
    Css.fontFamilies [ "Verdana", .value Css.sansSerif ]


stringFromError : Error -> String
stringFromError error =
    case error of
        FirestoreError { code, errorMessage } ->
            "There was an error with the database: " ++ errorMessage ++ " (code " ++ code ++ ")"

        ParsingError errorMessage ->
            "There was an error with how the data looks like: " ++ errorMessage


sortBarView : Remote Votes -> Remote TopicList -> Html Msg
sortBarView votes remoteTopics =
    let
        voteCountMap =
            voteCountMapFromVotes votes

        showButton =
            case remoteTopics of
                Got topics ->
                    if not (TopicList.isSorted voteCountMap topics) then
                        True

                    else
                        False

                _ ->
                    False

        visibility =
            if showButton then
                Css.visibility Css.visible

            else
                Css.visibility Css.hidden
    in
    div
        [ css
            [ visibility
            ]
        ]
        [ sortBar ]


sortBar : Html Msg
sortBar =
    sortButton


sortButton : Html Msg
sortButton =
    button [ css [ buttonStyle ], onClick SortTopics ] [ text "Sort" ]


type alias TopicViewModel a =
    { a | user : Remote User, isAdmin : Bool }


topicCard : TopicViewModel a -> TopicWithVotes -> Html Msg
topicCard model entry =
    let
        maybeVoteButton =
            case model.user of
                Got user ->
                    let
                        voteCount =
                            entry.votes
                                |> List.length

                        userAlreadyVoted =
                            List.any (\userId -> userId == user.id) entry.votes

                        state =
                            if userAlreadyVoted then
                                { action = RemoveUpvote
                                , saturation = 1
                                , lightness = 0.6
                                , border = Css.borderWidth (px 2)
                                , margin = px 0
                                }

                            else
                                { action = Upvote
                                , saturation = 0.4
                                , lightness = 0.9
                                , border = Css.batch []
                                , margin = px 1
                                }
                    in
                    [ button
                        [ onClick (state.action user entry.topic)
                        , css
                            [ buttonStyle
                            , Css.backgroundColor (Css.hsl primaryHue state.saturation state.lightness)
                            , state.border
                            , Css.margin state.margin
                            ]
                        ]
                        [ text
                            ("👍 " ++ String.fromInt voteCount)
                        ]
                    ]

                _ ->
                    []

        mayModify =
            if model.isAdmin then
                True

            else
                case model.user of
                    Loading ->
                        False

                    Got user ->
                        user.id == entry.topic.userId

        maybeDiscussButton =
            if model.isAdmin then
                [ button [ onClick (Discuss entry.topic.id), css [ buttonStyle ] ] [ text "Discuss" ] ]

            else
                []

        maybeDeleteButton =
            if mayModify then
                [ button
                    [ onClick (DeleteTopic entry.topic.id)
                    , css [ buttonStyle ]
                    ]
                    [ text "Delete" ]
                ]

            else
                []
    in
    card
        [ div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ text entry.topic.topic
            , div
                [ css
                    [ Css.marginTop (rem 1)
                    , Css.displayFlex
                    , Css.flexDirection Css.row
                    , Css.justifyContent Css.spaceBetween
                    ]
                ]
                (maybeVoteButton
                    ++ maybeDiscussButton
                    ++ maybeDeleteButton
                )
            ]
        ]


submitForm : Remote User -> String -> Html Msg
submitForm fetchedUser currentInput =
    case fetchedUser of
        Loading ->
            text "Logging you in…"

        Got user ->
            newTopic user currentInput


newTopic : User -> String -> Html Msg
newTopic user currentInput =
    form
        [ css [ Css.displayFlex, Css.flexDirection Css.column, Css.alignItems Css.flexStart ]
        , onSubmit (SaveTopic user)
        ]
        [ label [ css [ Css.displayFlex, Css.flexDirection Css.column, Css.width (pct 100) ] ]
            [ text "Your topic"
            , input
                [ value currentInput
                , onInput NewTopicInputChanged
                , css [ Css.padding2 (rem 0.5) (rem 0.5) ]
                ]
                []
            ]
        , input [ type_ "submit", value "Submit", css [ buttonStyle, Css.marginTop (rem 1) ] ] []
        ]


card : List (Html Msg) -> Html Msg
card content =
    div
        [ css
            [ Css.padding (rem 1)
            , Css.borderRadius (rem 0.5)
            , Css.boxShadow4 zero (rem 0.1) (rem 0.3) (Css.hsla 0 0 0 0.25)
            , Css.backgroundColor (Css.hsl 0 0 1)
            ]
        ]
        content


buttonStyle : Css.Style
buttonStyle =
    Css.batch
        [ Css.padding2 (rem 0.3) (rem 0.5)
        , Css.border3 (px 1) Css.solid (Css.hsl 0 0 0)
        , Css.borderRadius (rem 0.3)
        , Css.backgroundColor (Css.hsl 0 0 1)
        , Css.hover
            [ Css.property "filter" "brightness(90%)"
            ]
        ]


primaryHue : Float
primaryHue =
    49.1



--- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveFirestoreSubscriptions
        , receiveUser_ (Decode.decodeValue userDecoder >> UserReceived)
        , receiveError_ (Decode.decodeValue errorDecoder >> ErrorReceived)
        ]



--- PORTS


topicCollectionPath : Path
topicCollectionPath =
    [ "topics" ]


voteCollectionPath : Path
voteCollectionPath =
    [ "votes" ]


discussedDocPath : Path
discussedDocPath =
    [ "discussion", "discussed" ]


firestoreSubscriptionsCmd : Cmd Msg
firestoreSubscriptionsCmd =
    Cmd.batch
        [ subscribe { kind = Collection, path = topicCollectionPath, tag = TopicsTag }
        , subscribe { kind = Collection, path = voteCollectionPath, tag = VotesTag }
        , subscribe { kind = Doc, path = discussedDocPath, tag = DiscussedTag }
        ]


subscribe : SubscriptionInfo -> Cmd msg
subscribe info =
    encodeSubscriptionInfo info
        |> subscribe_


port subscribe_ : Encode.Value -> Cmd msg


type alias SubscriptionInfo =
    { kind : SubscriptionKind
    , path : Path
    , tag : SubscriptionTag
    }


encodeSubscriptionInfo : SubscriptionInfo -> Encode.Value
encodeSubscriptionInfo info =
    Encode.object
        [ ( "kind", encodeSubscriptionKind info.kind )
        , ( "path", encodePath info.path )
        , ( "tag", encodeSubscriptionTag info.tag )
        ]


type SubscriptionKind
    = Collection
    | Doc


encodeSubscriptionKind : SubscriptionKind -> Encode.Value
encodeSubscriptionKind kind =
    let
        raw =
            case kind of
                Collection ->
                    "collection"

                Doc ->
                    "doc"
    in
    Encode.string raw


subscriptionKindDecoder : Decoder SubscriptionKind
subscriptionKindDecoder =
    Decode.string
        |> Decode.andThen
            (\raw ->
                case raw of
                    "collection" ->
                        Decode.succeed Collection

                    "doc" ->
                        Decode.succeed Doc

                    _ ->
                        Decode.fail "Invalid subscription kind"
            )


type alias Path =
    List String


encodePath : Path -> Encode.Value
encodePath path =
    String.join "/" path
        |> Encode.string


type SubscriptionTag
    = TopicsTag
    | VotesTag
    | DiscussedTag


encodeSubscriptionTag : SubscriptionTag -> Encode.Value
encodeSubscriptionTag tag =
    let
        raw =
            case tag of
                TopicsTag ->
                    "topics"

                VotesTag ->
                    "votes"

                DiscussedTag ->
                    "discussed"
    in
    Encode.string raw


subscriptionTagDecoder : Decoder SubscriptionTag
subscriptionTagDecoder =
    Decode.string
        |> Decode.andThen
            (\raw ->
                case raw of
                    "topics" ->
                        Decode.succeed TopicsTag

                    "votes" ->
                        Decode.succeed VotesTag

                    "discussed" ->
                        Decode.succeed DiscussedTag

                    _ ->
                        Decode.fail "Invalid subscription kind"
            )


receiveFirestoreSubscriptions : Sub Msg
receiveFirestoreSubscriptions =
    receive_ parseFirestoreSubscription


port receive_ : (Encode.Value -> msg) -> Sub msg


parseFirestoreSubscription : Encode.Value -> Msg
parseFirestoreSubscription value =
    let
        decoded =
            Decode.decodeValue
                (Decode.field "tag" subscriptionTagDecoder
                    |> Decode.andThen
                        (\tag ->
                            let
                                dataDecoder : Decoder Msg
                                dataDecoder =
                                    case tag of
                                        TopicsTag ->
                                            topicsDecoder
                                                |> Decode.map TopicsReceived

                                        VotesTag ->
                                            votesDecoder
                                                |> Decode.map VotesReceived

                                        DiscussedTag ->
                                            discussedTopicDecoder
                                                |> Decode.map DiscussedTopicReceived
                            in
                            Decode.field "data" dataDecoder
                        )
                )
                value
    in
    case decoded of
        Ok msg ->
            msg

        Err error ->
            DecodeError error


type alias InsertDocInfo =
    { collectionPath : Path, data : Encode.Value }


insertDoc : InsertDocInfo -> Cmd msg
insertDoc info =
    Encode.object
        [ ( "path", encodePath info.collectionPath )
        , ( "data", info.data )
        ]
        |> insertDoc_


port insertDoc_ : Encode.Value -> Cmd msg


type alias SetDocInfo =
    { docPath : Path, data : Encode.Value }


setDoc : SetDocInfo -> Cmd msg
setDoc info =
    Encode.object
        [ ( "path", encodePath info.docPath )
        , ( "data", info.data )
        ]
        |> setDoc_


port setDoc_ : Encode.Value -> Cmd msg


deleteDocs : List Path -> Cmd msg
deleteDocs paths =
    Encode.object
        [ ( "paths", Encode.list encodePath paths )
        ]
        |> deleteDocs_


port deleteDocs_ : Encode.Value -> Cmd msg


port receiveUser_ : (Encode.Value -> msg) -> Sub msg


port receiveError_ : (Encode.Value -> msg) -> Sub msg



-- DECODERS


topicsDecoder : Decoder (List Topic)
topicsDecoder =
    Decode.list
        (Decode.map4
            (\id topic userId createdAt ->
                { id = id
                , topic = topic
                , userId = userId
                , createdAt = createdAt
                }
            )
            (Decode.field "id" Decode.string)
            (dataField "topic" Decode.string)
            (dataField "userId" Decode.string)
            (dataField "createdAt" (Decode.nullable timestampDecoder))
        )


dataField : String -> Decoder a -> Decoder a
dataField field decoder =
    Decode.field "data" (Decode.field field decoder)



-- old ports


userDecoder : Decoder User
userDecoder =
    Decode.field "id" Decode.string
        |> Decode.map (\id -> { id = id })


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


votesDecoder : Decoder Votes
votesDecoder =
    Decode.list
        (Decode.map2
            (\topicId userId ->
                ( topicId, userId )
            )
            (dataField "topicId" Decode.string)
            (dataField "userId" Decode.string)
        )
        |> Decode.map
            (List.foldl
                (\( topicId, userId ) dict ->
                    Dict.update topicId
                        (\maybeUsers ->
                            case maybeUsers of
                                Nothing ->
                                    Just [ userId ]

                                Just users ->
                                    Just (userId :: users)
                        )
                        dict
                )
                Dict.empty
            )


discussedTopicDecoder : Decoder (Maybe TopicId)
discussedTopicDecoder =
    Decode.maybe (Decode.field "topicId" Decode.string)


errorDecoder : Decoder Error
errorDecoder =
    Decode.map2
        (\code errorMessage ->
            FirestoreError { code = code, errorMessage = errorMessage }
        )
        (Decode.field "code" Decode.string)
        (Decode.field "message" Decode.string)


type alias TopicSubmission =
    { topic : String, userId : String }


topicEncoder : TopicSubmission -> TimestampField -> Encode.Value
topicEncoder { topic, userId } timestampField =
    Encode.object
        [ ( "topic", Encode.string topic )
        , ( "userId", Encode.string userId )
        , ( "createdAt", timestampField )
        ]


voteEncoder : Vote -> Encode.Value
voteEncoder vote =
    Encode.object
        [ ( "userId", Encode.string vote.userId )
        , ( "topicId", Encode.string vote.topicId )
        ]


discussedTopicEncoder : TopicId -> Encode.Value
discussedTopicEncoder topicId =
    Encode.object
        [ ( "topicId", Encode.string topicId )
        ]
