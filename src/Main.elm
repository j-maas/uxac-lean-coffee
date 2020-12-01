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


main : Program () Model Msg
main =
    Browser.element
        { view = view >> Html.toUnstyled
        , init = \_ -> init
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
    }


type Fetched a
    = Loading
    | Received a


type alias Question =
    { id : QuestionId
    , question : String
    , userId : String
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


init : ( Model, Cmd Msg )
init =
    ( { questions = Loading
      , votes = Loading
      , newQuestionInput = ""
      , user = Loading
      , error = Nothing
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
                    ( { model | questions = Received questions }, Cmd.none )

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
            , submitQuestionCmd { question = model.newQuestionInput, userId = user.id }
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
            , Css.padding2 (rem <| 1 - (verticalMargin / 2)) (rem 1)
            , Css.borderRadius (rem 0.5)
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
        ([ text question.question
         ]
            ++ maybeVoteButton
            ++ maybeDeleteButton
        )


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


submitQuestionCmd : QuestionSubmission -> Cmd msg
submitQuestionCmd submission =
    questionEncoder submission
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
        (Json.Decode.map3
            (\id question userId ->
                { id = id
                , question = question
                , userId = userId
                }
            )
            (Json.Decode.field "id" Json.Decode.string)
            (Json.Decode.field "question" Json.Decode.string)
            (Json.Decode.field "userId" Json.Decode.string)
        )


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


questionEncoder : QuestionSubmission -> Json.Encode.Value
questionEncoder { question, userId } =
    Json.Encode.object
        [ ( "question", Json.Encode.string question )
        , ( "userId", Json.Encode.string userId )
        ]


voteEncoder : User -> Question -> Json.Encode.Value
voteEncoder user question =
    Json.Encode.object
        [ ( "userId", Json.Encode.string user.id )
        , ( "questionId", Json.Encode.string question.id )
        ]
