port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Css exposing (auto, px, rem, zero)
import Css.Global as Global
import Html as PlainHtml
import Html.Styled as Html exposing (Html, button, div, form, h1, h2, input, label, p, text)
import Html.Styled.Attributes exposing (css, placeholder, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Time


main =
    Browser.element
        { view = view >> Html.toUnstyled
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL ----


type alias Model =
    { questions : Fetched (List Question)
    , votes : Fetched (List Vote)
    , newQuestionInput : String
    , user : Fetched User
    , error : Maybe Error

    {- This allows us to specify that a server should add a timestamp
       in a document's field.
       See https://firebase.google.com/docs/firestore/manage-data/add-data#server_timestamp.
    -}
    , timestampField : TimestampField
    }


type Fetched a
    = Loading
    | Received a


type alias Question =
    { id : QuestionId
    , question : String
    , userId : String
    , createdAt : Maybe Time.Posix
    }


type alias QuestionId =
    String


type alias Vote =
    { questionId : QuestionId
    , userId : UserId
    }


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
    { timestampField : TimestampField }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { questions = Loading
      , votes = Loading
      , newQuestionInput = ""
      , user = Loading
      , error = Nothing
      , timestampField = flags.timestampField
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UserReceived (Result Json.Decode.Error User)
    | QuestionsReceived (Result Json.Decode.Error (List Question))
    | VotesReceived (Result Json.Decode.Error (List Vote))
    | SaveQuestion User
    | DeleteQuestion QuestionId
    | Upvote User Question
    | RemoveUpvote User Question
    | ErrorReceived (Result Json.Decode.Error Error)
    | NewQuestionInputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserReceived result ->
            case result of
                Ok user ->
                    ( { model | user = Received user }, Cmd.none )

                Err error ->
                    ( { model | error = Just (ParsingError (Json.Decode.errorToString error)) }, Cmd.none )

        QuestionsReceived result ->
            case result of
                Ok questions ->
                    let
                        sortedQuestions =
                            List.sortWith
                                (\first second ->
                                    case ( first.createdAt, second.createdAt ) of
                                        ( Just firstTS, Just secondTS ) ->
                                            compare
                                                (Time.posixToMillis firstTS)
                                                (Time.posixToMillis secondTS)

                                        {- A missing timestamp means that the item was added locally.
                                           Then it should be sorted at the end of the, i.e., larger.
                                        -}
                                        ( Nothing, Just _ ) ->
                                            GT

                                        ( Just _, Nothing ) ->
                                            LT

                                        ( Nothing, Nothing ) ->
                                            EQ
                                )
                                questions
                    in
                    ( { model | questions = Received sortedQuestions }, Cmd.none )

                Err error ->
                    ( { model | error = Just (ParsingError (Json.Decode.errorToString error)) }, Cmd.none )

        VotesReceived result ->
            case result of
                Ok votes ->
                    ( { model | votes = Received votes }, Cmd.none )

                Err error ->
                    ( { model | error = Just (ParsingError (Json.Decode.errorToString error)) }, Cmd.none )

        ErrorReceived result ->
            case result of
                Ok value ->
                    ( { model | error = Just value }, Cmd.none )

                Err error ->
                    ( { model | error = Just (ParsingError (Json.Decode.errorToString error)) }, Cmd.none )

        SaveQuestion user ->
            ( { model | newQuestionInput = "" }
            , submitQuestionCmd model.timestampField { question = model.newQuestionInput, userId = user.id }
            )

        DeleteQuestion id ->
            ( model, deleteQuestionCmd id )

        Upvote user question ->
            ( model, submitVoteCmd user question )

        RemoveUpvote user question ->
            ( model, retractVoteCmd user question )

        NewQuestionInputChanged value ->
            ( { model | newQuestionInput = value }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        [ css
            [ bodyFont
            , Css.maxWidth (rem 32)
            , Css.margin2 zero auto
            ]
        ]
        ([ h1 [] [ text "UXAC Lean Coffee" ] ]
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
            ++ [ questionList model.questions model.user model.votes
               , submitForm model.user model.newQuestionInput
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


questionList : Fetched (List Question) -> Fetched User -> Fetched (List Vote) -> Html Msg
questionList fetchedQuestions currentUser votes =
    let
        verticalMargin =
            1

        contents =
            case fetchedQuestions of
                Loading ->
                    [ text "Loading questions…" ]

                Received questions ->
                    List.map (questionCard currentUser votes) questions
    in
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.backgroundColor (Css.hsl 49.1 0.2 0.95)
            , Css.padding2 zero (rem 1)
            , Css.borderRadius (rem 0.5)
            , Css.paddingTop (rem <| 1 - (verticalMargin / 2))
            , Css.paddingBottom (rem <| 1 - (verticalMargin / 2))
            , Global.children
                [ Global.everything [ Css.margin2 (rem <| verticalMargin / 2) zero ]
                ]
            ]
        ]
        contents


questionCard : Fetched User -> Fetched (List Vote) -> Question -> Html Msg
questionCard currentUser fetchedVotes question =
    let
        maybeVoteButton =
            case ( currentUser, fetchedVotes ) of
                ( Received user, Received votes ) ->
                    let
                        votesForThisQuestion =
                            List.filter (\{ questionId } -> questionId == question.id) votes

                        voteCount =
                            votesForThisQuestion
                                |> List.length

                        userAlreadyVoted =
                            List.any (\{ userId } -> userId == user.id) votesForThisQuestion

                        ( action, description ) =
                            if userAlreadyVoted then
                                ( RemoveUpvote, "Remove upvote" )

                            else
                                ( Upvote, "Upvote" )
                    in
                    [ button [ onClick (action user question) ]
                        [ text
                            (description
                                ++ " ("
                                ++ String.fromInt voteCount
                                ++ ")"
                            )
                        ]
                    ]

                _ ->
                    []

        mayModifyQuestion =
            case currentUser of
                Loading ->
                    False

                Received user ->
                    user.id == question.userId

        maybeDeleteButton =
            if mayModifyQuestion then
                [ button [ onClick (DeleteQuestion question.id) ] [ text "Delete" ] ]

            else
                []
    in
    card
        [ div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ text question.question
            , div
                []
                (maybeVoteButton
                    ++ maybeDeleteButton
                )
            ]
        ]


submitForm : Fetched User -> String -> Html Msg
submitForm fetchedUser currentInput =
    div [ css [ Css.marginTop (rem 1) ] ]
        [ case fetchedUser of
            Loading ->
                text "Connecting to database…"

            Received user ->
                newQuestion user currentInput
        ]


newQuestion : User -> String -> Html Msg
newQuestion user currentInput =
    form
        [ css [ Css.displayFlex, Css.flexDirection Css.column, Css.alignItems Css.flexStart ]
        , onSubmit (SaveQuestion user)
        ]
        [ label [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ text "Your question"
            , input [ value currentInput, onInput NewQuestionInputChanged ] []
            , input [ type_ "submit", value "Submit" ] []
            ]
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



--- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveUser (Json.Decode.decodeValue userDecoder >> UserReceived)
        , receiveQuestions (Json.Decode.decodeValue questionsDecoder >> QuestionsReceived)
        , receiveVotes (Json.Decode.decodeValue votesDecoder >> VotesReceived)
        , errorReceived (Json.Decode.decodeValue errorDecoder >> ErrorReceived)
        ]


type alias QuestionSubmission =
    { question : String, userId : String }


submitQuestionCmd : TimestampField -> QuestionSubmission -> Cmd msg
submitQuestionCmd timestampField submission =
    questionEncoder submission timestampField
        |> submitQuestion


deleteQuestionCmd : String -> Cmd msg
deleteQuestionCmd questionId =
    deleteQuestion questionId


submitVoteCmd : User -> Question -> Cmd msg
submitVoteCmd user question =
    voteEncoder user question
        |> submitVote


retractVoteCmd : User -> Question -> Cmd msg
retractVoteCmd user question =
    voteEncoder user question
        |> retractVote


port receiveUser : (Json.Encode.Value -> msg) -> Sub msg


port receiveQuestions : (Json.Encode.Value -> msg) -> Sub msg


port receiveVotes : (Json.Encode.Value -> msg) -> Sub msg


port errorReceived : (Json.Encode.Value -> msg) -> Sub msg


port submitQuestion : Json.Encode.Value -> Cmd msg


port deleteQuestion : String -> Cmd msg


port submitVote : Json.Encode.Value -> Cmd msg


port retractVote : Json.Encode.Value -> Cmd msg


userDecoder : Json.Decode.Decoder User
userDecoder =
    Json.Decode.field "id" Json.Decode.string
        |> Json.Decode.map (\id -> { id = id })


questionsDecoder : Json.Decode.Decoder (List Question)
questionsDecoder =
    Json.Decode.list
        (Json.Decode.map4
            (\id question userId createdAt ->
                { id = id
                , question = question
                , userId = userId
                , createdAt = createdAt
                }
            )
            (Json.Decode.field "id" Json.Decode.string)
            (Json.Decode.field "question" Json.Decode.string)
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


votesDecoder : Json.Decode.Decoder (List Vote)
votesDecoder =
    Json.Decode.list
        (Json.Decode.map2
            (\questionId userId ->
                { questionId = questionId
                , userId = userId
                }
            )
            (Json.Decode.field "questionId" Json.Decode.string)
            (Json.Decode.field "userId" Json.Decode.string)
        )


errorDecoder : Json.Decode.Decoder Error
errorDecoder =
    Json.Decode.map2
        (\code errorMessage ->
            FirestoreError { code = code, errorMessage = errorMessage }
        )
        (Json.Decode.field "code" Json.Decode.string)
        (Json.Decode.field "message" Json.Decode.string)


questionEncoder : QuestionSubmission -> TimestampField -> Json.Encode.Value
questionEncoder { question, userId } timestampField =
    Json.Encode.object
        [ ( "question", Json.Encode.string question )
        , ( "userId", Json.Encode.string userId )
        , ( "createdAt", timestampField )
        ]


voteEncoder : User -> Question -> Json.Encode.Value
voteEncoder user question =
    Json.Encode.object
        [ ( "userId", Json.Encode.string user.id )
        , ( "questionId", Json.Encode.string question.id )
        ]
