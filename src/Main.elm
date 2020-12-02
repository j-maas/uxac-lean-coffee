port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Css exposing (auto, pct, px, rem, zero)
import Css.Global as Global
import Dict exposing (Dict)
import Html as PlainHtml
import Html.Styled as Html exposing (Html, button, div, form, h1, h2, input, label, p, text, textarea)
import Html.Styled.Attributes exposing (css, placeholder, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
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
    { topics : Remote TopicList
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


type alias User =
    { id : UserId
    }


type alias UserId =
    String


type Error
    = FirestoreError { code : String, errorMessage : String }
    | ParsingError String


type alias TimestampField =
    Json.Decode.Value


type alias Flags =
    { timestampField : TimestampField
    , isAdmin : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { topics = Loading
      , votes = Loading
      , newTopicInput = ""
      , user = Loading
      , isAdmin = flags.isAdmin
      , error = Nothing
      , timestampField = flags.timestampField
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UserReceived (Result Json.Decode.Error User)
    | TopicsReceived (Result Json.Decode.Error (List Topic))
    | VotesReceived (Result Json.Decode.Error Votes)
    | SaveTopic User
    | DeleteTopic TopicId
    | SortTopics
    | Upvote User Topic
    | RemoveUpvote User Topic
    | ErrorReceived (Result Json.Decode.Error Error)
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
                    ( { model | error = Just (ParsingError (Json.Decode.errorToString error)) }, Cmd.none )

        TopicsReceived result ->
            case result of
                Ok topics ->
                    ( { model | topics = updateTopicList voteCountMap topics model.topics }, Cmd.none )

                Err error ->
                    ( { model | error = Just (ParsingError (Json.Decode.errorToString error)) }, Cmd.none )

        VotesReceived result ->
            case result of
                Ok votes ->
                    let
                        newModel =
                            case model.votes of
                                Loading ->
                                    { model
                                        | votes = Got votes

                                        -- If the votes are coming in for the first time, immediately sort the topics.
                                        , topics = Remote.map (TopicList.sort voteCountMap) model.topics
                                    }

                                Got _ ->
                                    { model | votes = Got votes }
                    in
                    ( newModel, Cmd.none )

                Err error ->
                    ( { model | error = Just (ParsingError (Json.Decode.errorToString error)) }, Cmd.none )

        ErrorReceived result ->
            case result of
                Ok value ->
                    ( { model | error = Just value }, Cmd.none )

                Err error ->
                    ( { model | error = Just (ParsingError (Json.Decode.errorToString error)) }, Cmd.none )

        SaveTopic user ->
            ( { model | newTopicInput = "" }
            , submitTopicCmd model.timestampField { topic = model.newTopicInput, userId = user.id }
            )

        DeleteTopic id ->
            ( model, deleteTopicCmd id )

        SortTopics ->
            ( { model | topics = Remote.map (TopicList.sort voteCountMap) model.topics }, Cmd.none )

        Upvote user topic ->
            ( model, submitVoteCmd user topic )

        RemoveUpvote user topic ->
            ( model, retractVoteCmd user topic )

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
            TopicList.from voteCountMap newTopics

        Got sortedList ->
            TopicList.update voteCountMap newTopics sortedList
    )
        |> Got



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
    in
    div
        [ css
            [ bodyFont
            , Css.maxWidth (rem 32)
            , Css.margin2 zero auto
            ]
        ]
        ([ h1 [] [ text heading ] ]
            ++ (case model.error of
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
               )
            ++ (let
                    showButton =
                        case model.topics of
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
                [ div [ css [ visibility ] ] [ sortBar ] ]
               )
            ++ [ div
                    [ css
                        [ Css.backgroundColor (Css.hsl primaryHue 0.2 0.95)
                        , Css.padding2 (rem 1) (rem 1)
                        , Css.borderRadius (rem 0.5)
                        ]
                    ]
                    ([ listing
                        (topicList model)
                     ]
                        ++ [ div
                                [ css [ Css.marginTop (rem 2) ]
                                ]
                                [ card <| [ submitForm model.user model.newTopicInput ] ]
                           ]
                    )
               ]
        )


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


sortBar : Html Msg
sortBar =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.alignItems Css.center
            ]
        ]
        [ text "The topics' order has changed.", div [ css [ Css.marginLeft (rem 1) ] ] [ sortButton ] ]


sortButton : Html Msg
sortButton =
    button [ css [ buttonStyle ], onClick SortTopics ] [ text "Sort" ]


listing : List (Html Msg) -> Html Msg
listing contents =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Global.children
                [ Global.everything
                    [ Global.adjacentSiblings
                        [ Global.everything
                            [ Css.marginTop (rem 1)
                            ]
                        ]
                    ]
                ]
            ]
        ]
        contents


topicList : Model -> List (Html Msg)
topicList model =
    case model.topics of
        Loading ->
            [ text "Loading topics…" ]

        Got topics ->
            List.map (topicCard model) (TopicList.toList topics)


topicCard : Model -> Topic -> Html Msg
topicCard model topic =
    let
        maybeVoteButton =
            case ( model.user, model.votes ) of
                ( Got user, Got votes ) ->
                    let
                        votesForThis =
                            Dict.get topic.id votes
                                |> Maybe.withDefault []

                        voteCount =
                            votesForThis
                                |> List.length

                        userAlreadyVoted =
                            List.any (\userId -> userId == user.id) votesForThis

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
                        [ onClick (state.action user topic)
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
                        user.id == topic.userId

        maybeDeleteButton =
            if mayModify then
                [ button
                    [ onClick (DeleteTopic topic.id)
                    , css [ buttonStyle ]
                    ]
                    [ text "Delete" ]
                ]

            else
                []
    in
    card
        [ div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ text topic.topic
            , div
                [ css
                    [ Css.marginTop (rem 1)
                    , Css.displayFlex
                    , Css.flexDirection Css.row
                    , Css.justifyContent Css.spaceBetween
                    ]
                ]
                (maybeVoteButton
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
        [ receiveUser (Json.Decode.decodeValue userDecoder >> UserReceived)
        , receiveTopics (Json.Decode.decodeValue topicsDecoder >> TopicsReceived)
        , receiveVotes (Json.Decode.decodeValue votesDecoder >> VotesReceived)
        , errorReceived (Json.Decode.decodeValue errorDecoder >> ErrorReceived)
        ]


type alias TopicSubmission =
    { topic : String, userId : String }


submitTopicCmd : TimestampField -> TopicSubmission -> Cmd msg
submitTopicCmd timestampField submission =
    topicEncoder submission timestampField
        |> submitTopic


deleteTopicCmd : String -> Cmd msg
deleteTopicCmd topicId =
    deleteTopic topicId


submitVoteCmd : User -> Topic -> Cmd msg
submitVoteCmd user topic =
    voteEncoder user topic
        |> submitVote


retractVoteCmd : User -> Topic -> Cmd msg
retractVoteCmd user topic =
    voteEncoder user topic
        |> retractVote


port receiveUser : (Json.Encode.Value -> msg) -> Sub msg


port receiveTopics : (Json.Encode.Value -> msg) -> Sub msg


port receiveVotes : (Json.Encode.Value -> msg) -> Sub msg


port errorReceived : (Json.Encode.Value -> msg) -> Sub msg


port submitTopic : Json.Encode.Value -> Cmd msg


port deleteTopic : String -> Cmd msg


port submitVote : Json.Encode.Value -> Cmd msg


port retractVote : Json.Encode.Value -> Cmd msg


userDecoder : Json.Decode.Decoder User
userDecoder =
    Json.Decode.field "id" Json.Decode.string
        |> Json.Decode.map (\id -> { id = id })


topicsDecoder : Json.Decode.Decoder (List Topic)
topicsDecoder =
    Json.Decode.list
        (Json.Decode.map4
            (\id topic userId createdAt ->
                { id = id
                , topic = topic
                , userId = userId
                , createdAt = createdAt
                }
            )
            (Json.Decode.field "id" Json.Decode.string)
            (Json.Decode.field "topic" Json.Decode.string)
            (Json.Decode.field "userId" Json.Decode.string)
            (Json.Decode.field "createdAt" (Json.Decode.nullable timestampDecoder))
        )


timestampDecoder : Json.Decode.Decoder Time.Posix
timestampDecoder =
    Json.Decode.map2
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
        (Json.Decode.field "seconds" Json.Decode.int)
        (Json.Decode.field "nanoseconds" Json.Decode.int)


votesDecoder : Json.Decode.Decoder Votes
votesDecoder =
    Json.Decode.list
        (Json.Decode.map2
            (\topicId userId ->
                ( topicId, userId )
            )
            (Json.Decode.field "topicId" Json.Decode.string)
            (Json.Decode.field "userId" Json.Decode.string)
        )
        |> Json.Decode.map
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


errorDecoder : Json.Decode.Decoder Error
errorDecoder =
    Json.Decode.map2
        (\code errorMessage ->
            FirestoreError { code = code, errorMessage = errorMessage }
        )
        (Json.Decode.field "code" Json.Decode.string)
        (Json.Decode.field "message" Json.Decode.string)


topicEncoder : TopicSubmission -> TimestampField -> Json.Encode.Value
topicEncoder { topic, userId } timestampField =
    Json.Encode.object
        [ ( "topic", Json.Encode.string topic )
        , ( "userId", Json.Encode.string userId )
        , ( "createdAt", timestampField )
        ]


voteEncoder : User -> Topic -> Json.Encode.Value
voteEncoder user topic =
    Json.Encode.object
        [ ( "userId", Json.Encode.string user.id )
        , ( "topicId", Json.Encode.string topic.id )
        ]
