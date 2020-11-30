port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, img, input, p, text)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ errorReceived (Json.Decode.decodeValue errorDecoder >> ErrorReceived)
        , receiveMessages (Json.Decode.decodeValue messageListDecoder >> MessagesReceived)
        ]


port errorReceived : (Json.Encode.Value -> msg) -> Sub msg


port saveMessage : Json.Encode.Value -> Cmd msg


port receiveMessages : (Json.Encode.Value -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { error : ErrorData
    , inputContent : String
    , messages : List String
    }


type alias MessageContent =
    { uid : String, content : String }


type alias ErrorData =
    { code : Maybe String, message : Maybe String, credential : Maybe String }


type alias UserData =
    { token : String, email : String, uid : String }


init : ( Model, Cmd Msg )
init =
    ( { error = emptyError
      , inputContent = ""
      , messages = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ErrorReceived (Result Json.Decode.Error ErrorData)
    | SaveMessage
    | InputChanged String
    | MessagesReceived (Result Json.Decode.Error (List String))


emptyError : ErrorData
emptyError =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Maybe.Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ErrorReceived result ->
            case result of
                Ok value ->
                    ( { model | error = value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        SaveMessage ->
            ( model, saveMessage <| messageEncoder model )

        InputChanged value ->
            ( { model | inputContent = value }, Cmd.none )

        MessagesReceived result ->
            case result of
                Ok value ->
                    ( { model | messages = value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )


messageEncoder : Model -> Json.Encode.Value
messageEncoder model =
    Json.Encode.object
        [ ( "content", Json.Encode.string model.inputContent )
        ]


messageToError : String -> ErrorData
messageToError message =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Just message }


errorPrinter : ErrorData -> String
errorPrinter errorData =
    Maybe.withDefault "" errorData.code ++ " " ++ Maybe.withDefault "" errorData.credential ++ " " ++ Maybe.withDefault "" errorData.message


errorDecoder : Json.Decode.Decoder ErrorData
errorDecoder =
    Json.Decode.succeed ErrorData
        |> Json.Decode.Pipeline.required "code" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "message" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "credential" (Json.Decode.nullable Json.Decode.string)


messagesDecoder =
    Json.Decode.decodeString (Json.Decode.list Json.Decode.string)


messageListDecoder : Json.Decode.Decoder (List String)
messageListDecoder =
    Json.Decode.succeed identity
        |> Json.Decode.Pipeline.required "messages" (Json.Decode.list Json.Decode.string)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "UXAC Lean Coffee" ]
        , div []
            [ input [ placeholder "Message to save", value model.inputContent, onInput InputChanged ] []
            , button [ onClick SaveMessage ] [ text "Save new message" ]
            ]
        , div []
            [ div [] <|
                List.map
                    (\m -> p [] [ text m ])
                    model.messages
            ]
        , h2 [] [ text <| errorPrinter model.error ]
        ]
