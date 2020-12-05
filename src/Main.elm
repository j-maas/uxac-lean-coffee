port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Css exposing (auto, num, pct, px, rem, zero)
import Css.Global as Global
import Css.Media as Media
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
    { inDiscussion : Maybe TopicId
    , topics : Remote TopicList
    , discussed : List TopicId
    , votes : Remote Votes
    , continuationVotes : List ContinuationVote
    , deadline : Maybe Time.Posix
    , now : Time.Posix
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


type alias ContinuationVote =
    { userId : UserId
    , vote : Continuation
    }


type Continuation
    = MoveOn
    | Stay
    | Abstain


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
    ( { inDiscussion = Nothing
      , topics = Loading
      , discussed = []
      , votes = Loading
      , continuationVotes = []
      , deadline = Just (Time.millisToPosix 1607192754000)
      , now = Time.millisToPosix 0
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
    | TopicInDiscussionReceived (Maybe TopicId)
    | ContinuationVotesReceived (List ContinuationVote)
    | DiscussedTopicsReceived (List TopicId)
    | SaveTopic User
    | DeleteTopic TopicId
    | Discuss TopicId
    | FinishDiscussionClicked
    | VoteAgainClicked
    | SortTopics
    | Upvote User Topic
    | RemoveUpvote User Topic
    | ContinuationVoteSent ContinuationVote
    | RemoveContinuationVote UserId
    | ErrorReceived (Result Decode.Error Error)
    | NewTopicInputChanged String
    | Tick Time.Posix


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

        ContinuationVotesReceived continuationVotes ->
            ( { model | continuationVotes = continuationVotes }, Cmd.none )

        TopicInDiscussionReceived topic ->
            ( { model | inDiscussion = topic }, Cmd.none )

        DiscussedTopicsReceived discussed ->
            ( { model | discussed = discussed }, Cmd.none )

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
            ( model, submitTopicInDiscussion topicId )

        FinishDiscussionClicked ->
            case model.inDiscussion of
                Just inDiscussion ->
                    let
                        cmds =
                            Cmd.batch
                                [ finishDiscussion inDiscussion
                                , clearContinuationVotes model.continuationVotes
                                ]
                    in
                    ( model, cmds )

                Nothing ->
                    ( model, Cmd.none )

        VoteAgainClicked ->
            case model.inDiscussion of
                Just inDiscussion ->
                    ( model, removeTopicInDiscussion )

                Nothing ->
                    ( model, Cmd.none )

        ContinuationVoteSent vote ->
            ( model, submitContinuationVote vote )

        RemoveContinuationVote userId ->
            ( model, retractContinuationVote userId )

        SortTopics ->
            ( { model | topics = Remote.map (TopicList.sort voteCountMap) model.topics }, Cmd.none )

        Upvote user topic ->
            ( model, submitVote (voteFrom user topic) )

        RemoveUpvote user topic ->
            ( model, retractVote (voteFrom user topic) )

        NewTopicInputChanged value ->
            ( { model | newTopicInput = value }, Cmd.none )

        Tick now ->
            ( { model | now = now }, Cmd.none )


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
        , doc =
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
           TODO: This smells like there might be a race condition when others add votes when we delete them. Instead, use batches. See https://github.com/Y0hy0h/uxac-lean-coffee/issues/37.
        -}
        votePaths =
            Remote.toMaybe votes
                |> Maybe.withDefault Dict.empty
                |> Dict.get topicId
                |> Maybe.withDefault []
                |> List.map (\userId -> topicVotePath { userId = userId, topicId = topicId })
    in
    deleteDocs (topicPath :: votePaths)


submitVote : Vote -> Cmd msg
submitVote vote =
    setDoc
        { docPath = topicVotePath vote
        , doc =
            topicVoteEncoder vote
        }


topicVotePath : Vote -> Path
topicVotePath ids =
    voteCollectionPath ++ [ ids.userId ++ ":" ++ ids.topicId ]


retractVote : Vote -> Cmd msg
retractVote vote =
    deleteDocs [ topicVotePath vote ]


submitContinuationVote : ContinuationVote -> Cmd msg
submitContinuationVote vote =
    setDoc
        { docPath = continuationVotePath vote.userId
        , doc = continuationVoteEncoder vote
        }


retractContinuationVote : UserId -> Cmd msg
retractContinuationVote userId =
    deleteDocs
        [ continuationVotePath userId
        ]


clearContinuationVotes : List ContinuationVote -> Cmd msg
clearContinuationVotes votes =
    deleteDocs
        (List.map
            (\vote ->
                continuationVotePath vote.userId
            )
            votes
        )


continuationVotePath : UserId -> Path
continuationVotePath userId =
    continuationVoteCollectionPath ++ [ userId ]


submitTopicInDiscussion : TopicId -> Cmd msg
submitTopicInDiscussion topicId =
    setDoc
        { docPath = inDiscussionDocPath
        , doc = topicIdEncoder topicId
        }


finishDiscussion : TopicId -> Cmd msg
finishDiscussion topicId =
    Cmd.batch
        [ deleteDocs [ inDiscussionDocPath ]
        , insertDoc
            { collectionPath = discussedCollectionPath
            , doc = topicIdEncoder topicId
            }
        ]


removeTopicInDiscussion : Cmd msg
removeTopicInDiscussion =
    deleteDocs [ inDiscussionDocPath ]



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

        ( inDiscussion, topicList, discussedList ) =
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
                , listItemSpacing
                ]
            ]
            ([ h1 [] [ text heading ] ]
                ++ errorView model.error
                ++ [ discussionView model inDiscussion model.continuationVotes model ]
                ++ [ discussedTopics model discussedList ]
                ++ [ topicEntry model.user model.newTopicInput ]
            )
        , topicsToVote model topicList (sortBarView model.votes model.topics)
        ]


limitWidth : Css.Style
limitWidth =
    Css.batch
        [ Css.maxWidth (rem 32)
        , Css.marginLeft auto
        , Css.marginRight auto
        ]


type alias TopicWithVotes =
    { topic : Topic
    , votes : List UserId
    }


processTopics :
    { a
        | inDiscussion : Maybe TopicId
        , topics : Remote TopicList
        , discussed : List TopicId
        , votes : Remote Votes
    }
    -> ( Maybe TopicWithVotes, Remote (List TopicWithVotes), List TopicWithVotes )
processTopics model =
    case model.topics of
        Loading ->
            ( Nothing, Loading, [] )

        Got topics ->
            let
                topicsWithVotes =
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

                ( maybeInDiscussion, remainingTopics ) =
                    Maybe.map (\topicId -> extract (\entry -> entry.topic.id == topicId) topicsWithVotes)
                        model.inDiscussion
                        |> Maybe.withDefault ( Nothing, topicsWithVotes )

                ( discussed, toVote ) =
                    List.partition (\t -> List.member t.topic.id model.discussed) remainingTopics
            in
            ( maybeInDiscussion, Got toVote, discussed )


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


discussionView :
    Credentials a
    -> Maybe TopicWithVotes
    -> List ContinuationVote
    -> { b | now : Time.Posix, deadline : Maybe Time.Posix }
    -> Html Msg
discussionView creds maybeDiscussedTopic continuationVotes times =
    div
        [ css
            [ borderRadius
            , Css.padding (rem 1)
            , Css.backgroundColor (Css.hsl primaryHue 1 0.5)
            , listItemSpacing
            ]
        ]
        ([ h2 [ css [ Css.margin zero ] ] [ text "In discussion" ]
         ]
            ++ (case maybeDiscussedTopic of
                    Just topic ->
                        [ topicToDiscussCard creds topic
                        ]

                    Nothing ->
                        [ div
                            [ css [ Css.opacity (num 0.5) ]
                            ]
                            [ card
                                [ div [ css [ Css.width (pct 100), Css.minHeight (rem 5) ] ]
                                    [ text "Currently there is no topic in discussion. Vote for one below."
                                    ]
                                ]
                            ]
                        ]
               )
            ++ (case times.deadline of
                    Just deadline ->
                        [ remainingTime { now = times.now, deadline = deadline } ]

                    Nothing ->
                        []
               )
            ++ [ continuationVote creds.user continuationVotes
               ]
        )


remainingTime : { now : Time.Posix, deadline : Time.Posix } -> Html Msg
remainingTime times =
    let
        difference =
            Time.posixToMillis times.deadline
                - Time.posixToMillis times.now

        differenceMinutes =
            round (toFloat difference / (60 * 1000))
    in
    div [] [ text (String.fromInt differenceMinutes ++ " minutes left…") ]


backgroundColor : Css.Style
backgroundColor =
    Css.backgroundColor (Css.hsl primaryHue 0.2 0.95)


discussedTopics : Credentials a -> List TopicWithVotes -> Html Msg
discussedTopics model topics =
    Html.details
        [ css
            [ containerPadding
            , backgroundColor
            , borderRadius
            ]
        ]
        [ Html.summary [] [ text "Discussed topics" ]
        , ol
            [ css
                [ listUnstyle
                , listItemSpacing
                , Css.marginTop (rem 1)
                ]
            ]
            (List.map
                (\topic ->
                    li []
                        [ finishedTopicCard model topic
                        ]
                )
                topics
            )
        ]


continuationVote : Remote User -> List ContinuationVote -> Html Msg
continuationVote remoteUser continuationVotes =
    div []
        (case remoteUser of
            Loading ->
                []

            Got user ->
                let
                    ( moveOnVotes, remainingVotes ) =
                        List.partition (\vote -> vote.vote == MoveOn) continuationVotes

                    ( stayVotes, abstainVotes ) =
                        List.partition (\vote -> vote.vote == Stay) remainingVotes

                    stayButton =
                        voteButton user
                            (List.map .userId stayVotes)
                            { upvote = ContinuationVoteSent { userId = user.id, vote = Stay }
                            , downvote = RemoveContinuationVote user.id
                            }
                            (\count -> text <| "Stay (" ++ String.fromInt count ++ ")")

                    abstainButton =
                        voteButton user
                            (List.map .userId abstainVotes)
                            { upvote = ContinuationVoteSent { userId = user.id, vote = Abstain }
                            , downvote = RemoveContinuationVote user.id
                            }
                            (\count -> text <| "Abstain (" ++ String.fromInt count ++ ")")

                    moveOnButton =
                        voteButton user
                            (List.map .userId moveOnVotes)
                            { upvote = ContinuationVoteSent { userId = user.id, vote = MoveOn }
                            , downvote = RemoveContinuationVote user.id
                            }
                            (\count -> text <| "Move on (" ++ String.fromInt count ++ ")")
                in
                [ div
                    [ css
                        [ Css.displayFlex
                        , Css.flexDirection Css.row
                        , Css.justifyContent Css.spaceBetween
                        ]
                    ]
                    [ stayButton, abstainButton, moveOnButton ]
                ]
        )


topicEntry : Remote User -> String -> Html Msg
topicEntry user newTopicInput =
    div
        [ css
            [ containerPadding
            , borderRadius
            , backgroundColor
            ]
        ]
        [ submitForm user newTopicInput ]


topicsToVote : Credentials a -> Remote (List TopicWithVotes) -> Html Msg -> Html Msg
topicsToVote model remoteTopics toolbar =
    div
        [ css
            [ backgroundColor
            , containerPadding
            , borderRadius
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
                        [ text "Suggested topics" ]
                    , toolbar
                    ]
                , let
                    breakpoint =
                        rem 40
                  in
                  ol
                    [ css
                        [ listUnstyle
                        , Css.displayFlex
                        , Css.flexDirection Css.column
                        , Media.withMedia [ Media.all [ Media.maxWidth breakpoint ] ]
                            [ listItemSpacing
                            ]
                        , Media.withMedia [ Media.all [ Media.minWidth breakpoint ] ]
                            [ Css.property
                                "display"
                                "grid"

                            -- TODO: Consider allowing smaller widths on narrow devices via media queries.
                            , Css.property "grid-template-columns" "repeat(auto-fill, minmax(20rem, 1fr))"
                            , Css.property "row-gap" "2rem"
                            , Css.property "column-gap" "1rem"
                            ]
                        ]
                    ]
                    (List.map
                        (\topic ->
                            li
                                []
                                [ topicToVoteCard model topic ]
                        )
                        topics
                    )
                ]
        )


listUnstyle : Css.Style
listUnstyle =
    Css.batch
        [ Css.padding zero
        , Css.margin zero
        , Css.listStyle Css.none
        ]


listItemSpacing : Css.Style
listItemSpacing =
    Global.children
        [ Global.everything
            [ Global.adjacentSiblings
                [ Global.everything
                    [ Css.marginTop (rem 1)
                    ]
                ]
            ]
        ]


borderRadius : Css.Style
borderRadius =
    Css.borderRadius (rem 0.5)


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


type alias Credentials a =
    { a | user : Remote User, isAdmin : Bool }


topicToDiscussCard : Credentials a -> TopicWithVotes -> Html Msg
topicToDiscussCard creds entry =
    let
        voteCount =
            List.length entry.votes

        mayMod =
            mayModify creds entry.topic.creator

        maybeVoteAgainButton =
            if creds.isAdmin then
                [ button
                    [ css [ buttonStyle ]
                    , onClick VoteAgainClicked
                    ]
                    [ text "Vote again" ]
                ]

            else
                []

        maybeFinishButton =
            if creds.isAdmin then
                [ button
                    [ css [ buttonStyle ]
                    , onClick FinishDiscussionClicked
                    ]
                    [ text "Finish" ]
                ]

            else
                []

        maybeDeleteButton =
            if creds.isAdmin then
                [ deleteButton entry.topic.id
                ]

            else
                []
    in
    topicCard
        ([ votesIndicator voteCount ]
            ++ maybeFinishButton
            ++ maybeVoteAgainButton
            ++ maybeDeleteButton
        )
        entry.topic.topic


topicToVoteCard : Credentials a -> TopicWithVotes -> Html Msg
topicToVoteCard creds entry =
    let
        maybeDiscussButton =
            if creds.isAdmin then
                [ button [ onClick (Discuss entry.topic.id), css [ buttonStyle ] ] [ text "Discuss" ] ]

            else
                []

        mayMod =
            mayModify creds entry.topic.creator

        maybeDeleteButton =
            if mayMod then
                [ deleteButton entry.topic.id ]

            else
                []
    in
    topicCard
        (maybeTopicVoteButton creds.user entry
            ++ maybeDiscussButton
            ++ maybeDeleteButton
        )
        entry.topic.topic


finishedTopicCard : Credentials a -> TopicWithVotes -> Html Msg
finishedTopicCard creds entry =
    let
        voteCount =
            List.length entry.votes

        mayMod =
            mayModify creds entry.topic.creator

        maybeDeleteButton =
            if mayMod then
                [ deleteButton entry.topic.id ]

            else
                []
    in
    topicCard
        ([ votesIndicator voteCount ]
            ++ maybeDeleteButton
        )
        entry.topic.topic


maybeTopicVoteButton : Remote User -> TopicWithVotes -> List (Html Msg)
maybeTopicVoteButton remoteUser entry =
    case remoteUser of
        Loading ->
            []

        Got user ->
            let
                upvoteMsg =
                    Upvote user entry.topic

                downvoteMsg =
                    RemoveUpvote user entry.topic
            in
            [ voteButton user
                entry.votes
                { upvote = upvoteMsg, downvote = downvoteMsg }
                votesText
            ]


voteButton : User -> List UserId -> { upvote : Msg, downvote : Msg } -> (Int -> Html Msg) -> Html Msg
voteButton user votes msgs content =
    let
        voteCount =
            List.length votes

        userAlreadyVoted =
            List.any (\userId -> userId == user.id) votes

        state =
            if userAlreadyVoted then
                { action = msgs.downvote
                , saturation = 1
                , lightness = 0.6
                , border = Css.borderWidth (px 2)
                , margin = px 0
                }

            else
                { action = msgs.upvote
                , saturation = 0.4
                , lightness = 0.9
                , border = Css.batch []
                , margin = px 1
                }
    in
    button
        [ onClick state.action
        , css
            [ buttonStyle
            , Css.backgroundColor (Css.hsl primaryHue state.saturation state.lightness)
            , state.border
            , Css.margin state.margin
            ]
        ]
        [ content voteCount ]


votesIndicator : Int -> Html Msg
votesIndicator count =
    div [ css [ smallBorderRadius, backgroundColor, buttonPadding ] ]
        [ votesText count
        ]


votesText : Int -> Html Msg
votesText count =
    text
        ("👍 " ++ String.fromInt count)


deleteButton : TopicId -> Html Msg
deleteButton topicId =
    button
        [ onClick (DeleteTopic topicId)
        , css [ buttonStyle ]
        ]
        [ text "Delete" ]


mayModify : Credentials a -> UserId -> Bool
mayModify creds creator =
    if creds.isAdmin then
        True

    else
        case creds.user of
            Loading ->
                False

            Got user ->
                user.id == creator


topicCard : List (Html Msg) -> String -> Html Msg
topicCard buttons topic =
    card
        [ div
            [ css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                ]
            ]
            [ text topic
            , div
                [ css
                    [ Css.marginTop (rem 1)
                    , Css.displayFlex
                    , Css.flexDirection Css.row
                    , Css.flexWrap Css.wrap
                    , Css.justifyContent Css.spaceBetween
                    ]
                ]
                buttons
            ]
        ]


submitForm : Remote User -> String -> Html Msg
submitForm fetchedUser currentInput =
    case fetchedUser of
        Loading ->
            text "Connecting…"

        Got user ->
            newTopic user currentInput


newTopic : User -> String -> Html Msg
newTopic user currentInput =
    form
        [ css [ Css.displayFlex, Css.flexDirection Css.column, Css.alignItems Css.flexStart ]
        , onSubmit (SaveTopic user)
        ]
        [ label [ css [ Css.displayFlex, Css.flexDirection Css.column, Css.width (pct 100) ] ]
            [ text "Add a topic"
            , input
                [ value currentInput
                , onInput NewTopicInputChanged
                , css
                    [ Css.padding2 (rem 0.5) (rem 0.5)
                    , Css.marginTop (rem 0.3)
                    , smallBorderRadius
                    , Css.border3 (px 1) Css.solid (Css.hsl 0 0 0)
                    , Css.boxShadow5 Css.inset (rem 0.1) (rem 0.1) (rem 0.1) (Css.hsla 0 0 0 0.15)
                    ]
                ]
                []
            ]
        , input [ type_ "submit", value "Submit", css [ buttonStyle, Css.marginTop (rem 1) ] ] []
        ]


smallBorderRadius : Css.Style
smallBorderRadius =
    Css.borderRadius (rem 0.3)


card : List (Html Msg) -> Html Msg
card content =
    div
        [ css
            [ Css.padding (rem 1)
            , borderRadius
            , Css.boxShadow4 zero (rem 0.1) (rem 0.3) (Css.hsla 0 0 0 0.25)
            , Css.backgroundColor (Css.hsl 0 0 1)
            ]
        ]
        content


buttonStyle : Css.Style
buttonStyle =
    Css.batch
        [ buttonPadding
        , Css.border3 (px 1) Css.solid (Css.hsl 0 0 0)
        , smallBorderRadius
        , Css.backgroundColor (Css.hsl 0 0 1)
        , Css.boxShadow4 (rem 0.1) (rem 0.1) (rem 0.1) (Css.hsla 0 0 0 0.15)
        , Css.hover
            [ Css.property "filter" "brightness(90%)"
            ]
        ]


buttonPadding : Css.Style
buttonPadding =
    Css.padding2 (rem 0.3) (rem 0.5)


primaryHue : Float
primaryHue =
    49.1



--- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([ receiveFirestoreSubscriptions
         , receiveUser_ (Decode.decodeValue userDecoder >> UserReceived)
         , receiveError_ (Decode.decodeValue errorDecoder >> ErrorReceived)
         ]
            ++ (case model.deadline of
                    Just _ ->
                        [ Time.every 1000 Tick ]

                    Nothing ->
                        []
               )
        )



--- PORTS


topicCollectionPath : Path
topicCollectionPath =
    [ "topics" ]


voteCollectionPath : Path
voteCollectionPath =
    [ "votes" ]


inDiscussionDocPath : Path
inDiscussionDocPath =
    [ "inDiscussion", "topic" ]


continuationVoteCollectionPath : Path
continuationVoteCollectionPath =
    [ "continuationVotes" ]


discussedCollectionPath : Path
discussedCollectionPath =
    [ "discussed" ]


firestoreSubscriptionsCmd : Cmd Msg
firestoreSubscriptionsCmd =
    Cmd.batch
        [ subscribe { kind = Collection, path = topicCollectionPath, tag = TopicsTag }
        , subscribe { kind = Collection, path = voteCollectionPath, tag = VotesTag }
        , subscribe { kind = Doc, path = inDiscussionDocPath, tag = InDiscussionTag }
        , subscribe { kind = Collection, path = continuationVoteCollectionPath, tag = ContinuationVotesTag }
        , subscribe { kind = Collection, path = discussedCollectionPath, tag = DiscussedTag }
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
    | InDiscussionTag
    | ContinuationVotesTag
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

                InDiscussionTag ->
                    "inDiscussion"

                ContinuationVotesTag ->
                    "continuationVotes"

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

                    "inDiscussion" ->
                        Decode.succeed InDiscussionTag

                    "continuationVotes" ->
                        Decode.succeed ContinuationVotesTag

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

                                        InDiscussionTag ->
                                            inDiscussionDecoder
                                                |> Decode.map TopicInDiscussionReceived

                                        ContinuationVotesTag ->
                                            continuationVotesDecoder
                                                |> Decode.map ContinuationVotesReceived

                                        DiscussedTag ->
                                            discussedDecoder
                                                |> Decode.map DiscussedTopicsReceived
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
    { collectionPath : Path, doc : Encode.Value }


insertDoc : InsertDocInfo -> Cmd msg
insertDoc info =
    Encode.object
        [ ( "path", encodePath info.collectionPath )
        , ( "doc", info.doc )
        ]
        |> insertDoc_


port insertDoc_ : Encode.Value -> Cmd msg


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
                , creator = userId
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


inDiscussionDecoder : Decoder (Maybe TopicId)
inDiscussionDecoder =
    Decode.maybe (Decode.field "topicId" Decode.string)


continuationVotesDecoder : Decoder (List ContinuationVote)
continuationVotesDecoder =
    Decode.list
        (Decode.map2
            (\userId vote ->
                { userId = userId
                , vote = vote
                }
            )
            (dataField "userId" Decode.string)
            (dataField "vote" continuationDecoder)
        )


discussedDecoder : Decoder (List TopicId)
discussedDecoder =
    Decode.list (dataField "topicId" Decode.string)


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


topicVoteEncoder : Vote -> Encode.Value
topicVoteEncoder vote =
    Encode.object
        [ ( "userId", Encode.string vote.userId )
        , ( "topicId", Encode.string vote.topicId )
        ]


topicIdEncoder : TopicId -> Encode.Value
topicIdEncoder topicId =
    Encode.object
        [ ( "topicId", Encode.string topicId )
        ]


continuationVoteEncoder : ContinuationVote -> Encode.Value
continuationVoteEncoder vote =
    Encode.object
        [ ( "userId", Encode.string vote.userId )
        , ( "vote", encodeContinuation vote.vote )
        ]


encodeContinuation : Continuation -> Encode.Value
encodeContinuation continuation =
    let
        string =
            case continuation of
                MoveOn ->
                    "moveOn"

                Stay ->
                    "stay"

                Abstain ->
                    "abstain"
    in
    Encode.string string


continuationDecoder : Decoder Continuation
continuationDecoder =
    Decode.string
        |> Decode.andThen
            (\raw ->
                case raw of
                    "moveOn" ->
                        Decode.succeed MoveOn

                    "stay" ->
                        Decode.succeed Stay

                    "abstain" ->
                        Decode.succeed Abstain

                    _ ->
                        Decode.fail "Invalid continuation vote"
            )
