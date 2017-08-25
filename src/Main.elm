module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput, onClick)
import RemoteData exposing (WebData)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


-- MODEL


type alias WordData =
    { word : String
    , def : List (Maybe String)
    , selected : Bool
    }


type alias Vocab =
    { wordfind : List WordData
    , cambridge : List WordData
    , synonyms : List WordData
    , antonyms : List WordData
    }


type alias Model =
    { content : String
    , wordfindDef : Bool
    , result : WebData Vocab
    }


init : ( Model, Cmd Msg )
init =
    ( { content = ""
      , wordfindDef = True
      , result = RemoteData.NotAsked
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = Change String
    | Curl
    | ToggleDef
    | ToggleSelect String
    | SelectAll
    | DiselectAll
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


countHeader : List a -> String -> Html Msg
countHeader theList src =
    h5 []
        [ theList
            |> List.length
            |> toString
            |> (++) ("Found " ++ src ++ " ")
            |> text
        ]


vocabHtml : Bool -> Vocab -> Html Msg
vocabHtml showDef vocab =
    -- notice that 2nd [] here is replaced with (List.map ...)
    div []
        [ countHeader vocab.wordfind "wordfind"
        , vocab.wordfind
            |> List.map (wordDataRow showDef)
            |> vocabTable
        , countHeader vocab.cambridge "cambridge"
        , vocab.cambridge
            |> List.map (wordDataRow showDef)
            |> vocabTable
        , countHeader vocab.synonyms "synonyms"
        , vocab.synonyms
            |> List.map (wordDataRow False)
            |> vocabTable
        , countHeader vocab.antonyms "antonyms"
        , vocab.antonyms
            |> List.map (wordDataRow False)
            |> vocabTable
        ]


vocabTable : List (Html Msg) -> Html Msg
vocabTable rows =
    table []
        [ thead []
            [ tr []
                [ th []
                    [ text "Ignore", button [ onClick DiselectAll ] [ text "Diselect all" ] ]
                , th []
                    [ text "Save", button [ onClick SelectAll ] [ text "Select all" ] ]
                ]
            ]
        , tbody []
            rows
        ]


wordDataRow : Bool -> WordData -> Html Msg
wordDataRow showDef wordData =
    let
        ( show, hide ) =
            ( td []
                [ button [ onClick (ToggleSelect wordData.word) ] [ text ">" ]
                , (if showDef then
                    wordData.def
                        |> List.map (\defItem -> wordDataDef defItem)
                        |> toString
                        |> (++) wordData.word
                        |> text
                   else
                    text wordData.word
                  )
                ]
            , td []
                [ text "" ]
            )
    in
        case wordData.selected of
            True ->
                tr [] [ hide, show ]

            False ->
                tr [] [ show, hide ]


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


selectWordData : Vocab -> String -> Vocab
selectWordData vocab toSelect =
    let
        apply =
            List.map
                (\wordDataItem ->
                    if wordDataItem.word == toSelect then
                        { wordDataItem | selected = flip wordDataItem.selected }
                    else
                        wordDataItem
                )
    in
        { vocab
            | wordfind = apply vocab.wordfind
            , cambridge = apply vocab.cambridge
            , synonyms = apply vocab.synonyms
            , antonyms = apply vocab.antonyms
        }


setSelect : Vocab -> Bool -> Vocab
setSelect vocab boolean =
    let
        apply =
            List.map
                (\wordDataItem ->
                    { wordDataItem | selected = boolean }
                )
    in
        { vocab
            | wordfind = apply vocab.wordfind
            , cambridge = apply vocab.cambridge
            , synonyms = apply vocab.synonyms
            , antonyms = apply vocab.antonyms
        }


remoteDataUpdate : Model -> (Vocab -> Vocab) -> ( Model, Cmd Msg )
remoteDataUpdate model apply =
    let
        ( webdata, cmd ) =
            model.result
                |> RemoteData.update (\vocab -> ( apply vocab, Cmd.none ))
    in
        ( { model | result = webdata }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( { model | content = newContent }, Cmd.none )

        Curl ->
            ( { model | result = RemoteData.Loading }, curl model.content )

        ToggleDef ->
            ( { model | wordfindDef = (flip model.wordfindDef) }, Cmd.none )

        ToggleSelect word ->
            remoteDataUpdate model (\vocab -> selectWordData vocab word)

        SelectAll ->
            remoteDataUpdate model (\vocab -> setSelect vocab True)

        DiselectAll ->
            remoteDataUpdate model (\vocab -> setSelect vocab False)

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
        |> required "synonyms" (Decode.list wordDataDecoder)
        |> required "antonyms" (Decode.list wordDataDecoder)


wordDataDecoder : Decode.Decoder WordData
wordDataDecoder =
    decode WordData
        |> required "word" Decode.string
        |> optional "def" (Decode.list (Decode.maybe Decode.string)) []
        |> hardcoded False



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
