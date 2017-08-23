module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput, onClick)
import RemoteData exposing (WebData)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional)


-- MODEL


type alias WordData =
    { word : String
    , def : List (Maybe String)
    }


type alias Vocab =
    { wordfind : List WordData
    , cambridge : List WordData
    }


type alias Model =
    { content : String
    , wordfindDef : Bool
    , result : WebData Vocab
    }


init : ( Model, Cmd Msg )
init =
    ( { content = "", wordfindDef = True, result = RemoteData.NotAsked }, Cmd.none )



-- MESSAGES


type Msg
    = Change String
    | Curl
    | ToggleDef
    | OnResponse (WebData Vocab)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to reverse", onInput Change ] []
        , button [ onClick Curl ] [ text "Look!" ]
        , button [ onClick ToggleDef ] [ text "Definition" ]
        , div [] [ maybeResult model.wordfindDef model.result ]
        ]


maybeResult : Bool -> WebData Vocab -> Html Msg
maybeResult showDef response =
    case response of
        RemoteData.NotAsked ->
            text "Look up something ..."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success vocab ->
            vocabHtml showDef vocab

        RemoteData.Failure error ->
            text (toString error)


vocabHtml : Bool -> Vocab -> Html Msg
vocabHtml showDef vocab =
    -- notice that 2nd [] here is replaced with (List.map ...)
    div []
        [ h5 []
            [ vocab.wordfind
                |> List.length
                |> toString
                |> (++) "Found wordfind "
                |> text
            ]
        , ul []
            (vocab.wordfind
                |> List.map (wordDataHtml showDef)
            )
        , h5 []
            [ vocab.cambridge
                |> List.length
                |> toString
                |> (++) "Found cambridge "
                |> text
            ]
        , ul []
            (vocab.cambridge
                |> List.map wordHtml
            )
        ]


wordDataHtml : Bool -> WordData -> Html Msg
wordDataHtml showDef wordData =
    li []
        [ if showDef then
            wordData.def
                |> List.map (\defItem -> wordDataDef defItem)
                |> toString
                |> (++) wordData.word
                |> text
          else
            text wordData.word
        ]


wordHtml : WordData -> Html Msg
wordHtml wordData =
    li []
        [ text wordData.word
        ]


wordDataDef : Maybe String -> String
wordDataDef def =
    case def of
        Nothing ->
            "error"

        Just defStr ->
            defStr



-- UPDATE


flip : Bool -> Bool
flip b =
    case b of
        True ->
            False

        False ->
            True


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( { model | content = newContent }, Cmd.none )

        Curl ->
            ( { model | result = RemoteData.Loading }, curl model.content )

        ToggleDef ->
            ( { model | wordfindDef = (flip model.wordfindDef) }, Cmd.none )

        OnResponse response ->
            ( { model | result = response }, Cmd.none )



-- COMMANDS


curl : String -> Cmd Msg
curl query =
    Http.get ("http://localhost:4000/lookupword?w=" ++ query) vocabDecoder
        |> RemoteData.sendRequest
        |> Cmd.map OnResponse



--https://github.com/NoRedInk/elm-decode-pipeline


vocabDecoder : Decode.Decoder Vocab
vocabDecoder =
    decode Vocab
        |> required "wordfind" (Decode.list wordDataDecoder)
        |> required "cambridge" (Decode.list wordDataDecoder)


wordDataDecoder : Decode.Decoder WordData
wordDataDecoder =
    decode WordData
        |> required "word" Decode.string
        |> optional "def" (Decode.list (Decode.maybe Decode.string)) []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
