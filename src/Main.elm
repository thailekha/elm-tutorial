module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput, onClick)
import RemoteData exposing (WebData)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)


-- MODEL

type alias Vocab = 
    { wordfind : List String
    }

type alias Model =
    { content : String
    , result : WebData (Vocab)
    }


init : ( Model, Cmd Msg )
init =
    ( { content = "", result = RemoteData.NotAsked }, Cmd.none )



-- MESSAGES


type Msg
    = Change String
    | Curl
    | OnResponse (WebData (Vocab))



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Text to reverse", onInput Change ] []
    , button [ onClick Curl ] [ text "Look!" ]
    , div [] [maybeResult model.result]
    ]


maybeResult : (WebData Vocab) -> Html Msg
maybeResult response =
    case response of
        RemoteData.NotAsked ->
            text "Look up something ..."
        RemoteData.Loading ->
            text "Loading..."
        RemoteData.Success vocab ->
            vocabList vocab
        RemoteData.Failure error ->
            text (toString error)


vocabList : Vocab -> Html Msg
vocabList vocab =
    -- notice that 2nd [] here is replaced with (List.map ...)
    div [] 
    [
      h5 [] 
      [
        vocab.wordfind 
            |> List.length
            |> toString 
            |> (++) "Found "
            |> text
      ]
    , ul [] 
      (
        vocab.wordfind
            |> List.map (\word -> li [] [text word])
      )
    ]


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ({ model | content = newContent }, Cmd.none )
        Curl ->
            ({ model | result = RemoteData.Loading }, curl model.content )
        OnResponse response ->
            ({ model | result = response }, Cmd.none )


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
        |> required "wordfind" (Decode.list Decode.string)


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