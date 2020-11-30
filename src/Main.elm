port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Css exposing (auto, px, rem, zero)
import Css.Global as Global
import Html as PlainHtml
import Html.Styled as Html exposing (Html, button, div, form, h1, h2, input, label, p, text)
import Html.Styled.Attributes exposing (css, placeholder, src, type_, value)
import Html.Styled.Events exposing (onInput, onSubmit)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ errorReceived (Json.Decode.decodeValue errorDecoder >> ErrorReceived)
        , receiveQuestions (Json.Decode.decodeValue questionsDecoder >> QuestionsReceived)
        ]


port errorReceived : (Json.Encode.Value -> msg) -> Sub msg


port submitQuestion : Json.Encode.Value -> Cmd msg


port receiveQuestions : (Json.Encode.Value -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { questions : List Question
    , newQuestionInput : String
    , error : Maybe Error
    }


type alias Question =
    { question : String
    }


type Error
    = FirestoreError { code : String, errorMessage : String }
    | ParsingError String


init : ( Model, Cmd Msg )
init =
    ( { questions = []
      , newQuestionInput = ""
      , error = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ErrorReceived (Result Json.Decode.Error Error)
    | SaveMessage
    | NewQuestionInputChanged String
    | QuestionsReceived (Result Json.Decode.Error (List Question))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ErrorReceived result ->
            case result of
                Ok value ->
                    ( { model | error = Just value }, Cmd.none )

                Err error ->
                    ( { model | error = Just (ParsingError (Json.Decode.errorToString error)) }, Cmd.none )

        SaveMessage ->
            ( { model | newQuestionInput = "" }, submitQuestion <| questionEncoder model )

        NewQuestionInputChanged value ->
            ( { model | newQuestionInput = value }, Cmd.none )

        QuestionsReceived result ->
            case result of
                Ok questions ->
                    ( { model | questions = questions }, Cmd.none )

                Err error ->
                    ( { model | error = Just (ParsingError (Json.Decode.errorToString error)) }, Cmd.none )


questionEncoder : Model -> Json.Encode.Value
questionEncoder model =
    Json.Encode.object
        [ ( "question", Json.Encode.string model.newQuestionInput )
        ]


errorDecoder : Json.Decode.Decoder Error
errorDecoder =
    Json.Decode.map2
        (\code errorMessage ->
            FirestoreError { code = code, errorMessage = errorMessage }
        )
        (Json.Decode.field "code" Json.Decode.string)
        (Json.Decode.field "message" Json.Decode.string)


questionsDecoder : Json.Decode.Decoder (List Question)
questionsDecoder =
    Json.Decode.list
        (Json.Decode.map
            (\question ->
                { question = question
                }
            )
            (Json.Decode.field "question" Json.Decode.string)
        )



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
            ++ [ let
                    verticalMargin =
                        1
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
                    (List.map questionCard model.questions)
               , div [ css [ Css.marginTop (rem 1) ] ] [ newQuestion model.newQuestionInput ]
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


questionCard : Question -> Html Msg
questionCard question =
    card [ text question.question ]


newQuestion : String -> Html Msg
newQuestion currentInput =
    form
        [ css [ Css.displayFlex, Css.flexDirection Css.column, Css.alignItems Css.flexStart ]
        , onSubmit SaveMessage
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
