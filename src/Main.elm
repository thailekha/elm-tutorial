module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, href)
import Html.Events exposing (onInput, onClick)
import RemoteData exposing (WebData)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Navigation exposing (Location)
import UrlParser


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
    , route : Route
    }


type Route
    = Contain
    | Family
    | Synonyms
    | Antonyms
    | NotFoundRoute


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map Contain UrlParser.top
        , UrlParser.map Family (UrlParser.s "family")
        , UrlParser.map Synonyms (UrlParser.s "synonyms")
        , UrlParser.map Antonyms (UrlParser.s "antonyms")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (UrlParser.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


initModel : Route -> Model
initModel route =
    { content = ""
    , wordfindDef = True
    , result = RemoteData.NotAsked
    , route = route
    }


init : Location -> ( Model, Cmd Msg )
init location =
    ( location
        |> parseLocation
        |> initModel
    , Cmd.none
    )



-- MESSAGES


type Msg
    = OnLocationChange Location
    | Change String
    | Curl
    | ToggleDef
    | ToggleSelect String
    | SelectAll
    | DiselectAll
    | OnResponse (WebData Vocab)



-- VIEW
--highlightNav : Route -> Model -> string
--highlightNav route model =


nav : Model -> Html Msg
nav model =
    --make NavButton with selected boolean field to refactor this code
    div []
        [ a [ href "#/" ]
            [ (case model.route of
                Contain ->
                    "*"

                _ ->
                    ""
              )
                |> (++) " Contain "
                |> text
            ]
        , a [ href "#/family" ]
            [ (case model.route of
                Family ->
                    "*"

                _ ->
                    ""
              )
                |> (++) " Family "
                |> text
            ]
        , a [ href "#/synonyms" ]
            [ (case model.route of
                Synonyms ->
                    "*"

                _ ->
                    ""
              )
                |> (++) " Synonyms "
                |> text
            ]
        , a [ href "#/antonyms" ]
            [ (case model.route of
                Antonyms ->
                    "*"

                _ ->
                    ""
              )
                |> (++) " Antonyms "
                |> text
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to reverse", onInput Change ] []
        , button [ onClick Curl ] [ text "Look!" ]
        , button [ onClick ToggleDef ] [ text "Definition" ]
        , nav model
        , div [] [ maybeResult model model.result ]
        ]


maybeResult : Model -> WebData Vocab -> Html Msg
maybeResult model response =
    case response of
        RemoteData.NotAsked ->
            text "Look up something ..."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success vocab ->
            vocabHtml model.route model.wordfindDef vocab

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


vocabHtml : Route -> Bool -> Vocab -> Html Msg
vocabHtml route showDef vocab =
    -- notice that 2nd [] here is replaced with (List.map ...)
    div []
        (case route of
            Contain ->
                [ countHeader vocab.wordfind "wordfind"
                , vocab.wordfind
                    |> List.map (wordDataRow showDef)
                    |> vocabTable
                ]

            Family ->
                [ countHeader vocab.cambridge "cambridge"
                , vocab.cambridge
                    |> List.map (wordDataRow showDef)
                    |> vocabTable
                ]

            Synonyms ->
                [ countHeader vocab.synonyms "synonyms"
                , vocab.synonyms
                    |> List.map (wordDataRow False)
                    |> vocabTable
                ]

            Antonyms ->
                [ countHeader vocab.antonyms "antonyms"
                , vocab.antonyms
                    |> List.map (wordDataRow False)
                    |> vocabTable
                ]

            NotFoundRoute ->
                [ text "Route not found"
                ]
        )


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
        OnLocationChange location ->
            ( { model | route = (parseLocation location) }, Cmd.none )

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
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
