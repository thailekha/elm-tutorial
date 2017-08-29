module Main exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import String
import Html exposing (..)
import RemoteData exposing (WebData)
import Http
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Tabs as Tabs
import Material.List as Lists
import Material.Icon as Icon
import Material.Color as Color
import Material.Helpers exposing (map1st, map2nd, delay, pure, cssTransitionStep)
import Material.Snackbar as Snackbar
import Material.Options as Options exposing (css)
import Material.Options exposing (Property)
import Navigation
import RouteUrl as Routing


--see https://github.com/debois/elm-mdl/blob/master/demo/Demo.elm
--see https://github.com/debois/elm-mdl/blob/master/demo/Demo/Tabs.elm
-- CONSTANTS


colorSelected : Property c m
colorSelected =
    Color.background (Color.color Color.Blue Color.A700)


colorNotSelected : Property c m
colorNotSelected =
    Color.background (Color.color Color.Grey Color.A700)



-- MODEL


type alias WordData =
    { word : String
    , def : List (Maybe String)
    , selected : Bool
    }


type alias Vocab =
    { query : String
    , wordfind : List WordData
    , cambridge : List WordData
    , synonyms : List WordData
    , antonyms : List WordData
    }


vocabField : Vocab -> String -> List WordData
vocabField vocab key =
    case key of
        "wordfind" ->
            vocab.wordfind

        "cambridge" ->
            vocab.cambridge

        "synonyms" ->
            vocab.synonyms

        "antonyms" ->
            vocab.antonyms

        _ ->
            []


type alias Model =
    { content : String
    , wordfindDef : Bool
    , result : WebData Vocab
    , mdl : Material.Model
    , selectedTab : Int
    , snackbar : Snackbar.Model String
    }


init : ( Model, Cmd Msg )
init =
    ( { content = ""
      , wordfindDef = True
      , result = RemoteData.NotAsked
      , mdl = Material.model
      , selectedTab = 1
      , snackbar = Snackbar.model
      }
    , Cmd.none
    )


tabs : List ( String, String, Model -> Vocab -> Html Msg )
tabs =
    [ ( "Contains", "contains", vocabView "wordfind" )
    , ( "Family", "family", vocabView "cambridge" )
    , ( "Synonyms", "synonyms", vocabView "synonyms" )
    , ( "Antonyms", "antonyms", vocabView "antonyms" )
    ]



-- ENCODERS, DECODERS
--https://github.com/NoRedInk/elm-decode-pipeline


vocabDecoder : String -> Decode.Decoder Vocab
vocabDecoder query =
    decode Vocab
        |> hardcoded query --query
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


filterAndEncodeWordData : List WordData -> Encode.Value
filterAndEncodeWordData wordData =
    wordData
        |> List.filter (\i -> i.selected)
        |> List.map (\i -> encodeWordData i)
        |> Encode.list


encodeVocab : Vocab -> Encode.Value
encodeVocab vocab =
    Encode.object
        [ ( "query", Encode.string vocab.query )
        , ( "wordfind", filterAndEncodeWordData vocab.wordfind )
        , ( "cambridge", filterAndEncodeWordData vocab.cambridge )
        , ( "synonyms", filterAndEncodeWordData vocab.synonyms )
        , ( "antonyms", filterAndEncodeWordData vocab.antonyms )
        ]


encodeWordData : WordData -> Encode.Value
encodeWordData wordData =
    Encode.object
        [ ( "word", Encode.string wordData.word )
        , ( "def", List.map (\i -> i |> wordDataDef |> Encode.string) wordData.def |> Encode.list )
        ]



-- MESSAGES


type Msg
    = Change String
    | Curl
    | ToggleDef
    | ToggleSelect String
    | SelectAll
    | DiselectAll
    | OnResponse (WebData Vocab)
    | ReqSave
    | SaveResponse (WebData String)
    | Mdl (Material.Msg Msg)
    | SelectTab Int
      --| AddSnackbar
    | Snackbar (Snackbar.Msg String)



-- ROUTING


tabUrls : Array String
tabUrls =
    List.map (\( _, x, _ ) -> x) tabs |> Array.fromList


tabViews : Array (Model -> Vocab -> Html Msg)
tabViews =
    List.map (\( _, _, v ) -> v) tabs |> Array.fromList


urlTabs : Dict String Int
urlTabs =
    List.indexedMap (\idx ( _, x, _ ) -> ( x, idx )) tabs |> Dict.fromList


urlOf : Model -> String
urlOf model =
    "#" ++ (Array.get model.selectedTab tabUrls |> Maybe.withDefault "")


delta2url : Model -> Model -> Maybe Routing.UrlChange
delta2url model1 model2 =
    if model1.selectedTab /= model2.selectedTab then
        { entry = Routing.NewEntry
        , url = urlOf model2
        }
            |> Just
    else
        Nothing


location2messages : Navigation.Location -> List Msg
location2messages location =
    [ case location.hash |> String.dropLeft 1 of
        "" ->
            SelectTab 0

        x ->
            Dict.get x urlTabs
                |> Maybe.withDefault -1
                |> SelectTab
    ]



-- VIEW


tabLabels : List (Tabs.Label Msg)
tabLabels =
    List.map (\( x, _, _ ) -> Tabs.label [] [ text x ]) tabs


nav : Model -> Html Msg
nav model =
    Tabs.render Mdl
        [ 0 ]
        model.mdl
        [ Tabs.ripple
        , Tabs.onSelectTab SelectTab
        , Tabs.activeTab model.selectedTab
        ]
        tabLabels
        []


buttonMdl : Model -> Int -> Msg -> String -> Html Msg
buttonMdl model index cb display =
    Button.render Mdl
        [ index ]
        model.mdl
        -- onClick cannot be used multiple times
        [ Options.onClick cb ]
        [ text display ]



----doesn't work
--patchTextfield : (x -> Html Msg) -> Html Msg
--patchTextfield tf =
--    tf "some shit"


view : Model -> Html Msg
view model =
    div []
        [ --input [ placeholder "Text to reverse", onInput Change ] []
          (Textfield.render Mdl
            [ 0 ]
            model.mdl
            [ Textfield.label "word"
            , Textfield.floatingLabel
            , Options.onInput Change
            ]
          )
            "JUST A PATCH HERE"
        , buttonMdl model 1 Curl "Look!"
        , buttonMdl model 2 ToggleDef "Definition"
        , buttonMdl model 2 ReqSave "Save"
        , nav model
        , div []
            [ maybeResult model model.result
            , Snackbar.view model.snackbar |> Html.map Snackbar
            ]
        ]
        |> Material.Scheme.top


maybeResult : Model -> WebData Vocab -> Html Msg
maybeResult model response =
    case response of
        RemoteData.NotAsked ->
            text "Look up something ..."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success vocab ->
            --vocabHtml model.route model.wordfindDef vocab
            (Array.get model.selectedTab tabViews |> Maybe.withDefault e404) model vocab

        RemoteData.Failure error ->
            text (toString error)


e404 : x -> Vocab -> Html Msg
e404 _ vocab =
    div []
        [ text "route not found" ]


vocabView : String -> Model -> Vocab -> Html Msg
vocabView field model vocab =
    div []
        [ h5 []
            [ vocabField vocab field
                |> List.length
                |> toString
                |> (++) ("Found " ++ field ++ " ")
                |> text
            ]
        , buttonMdl model 0 DiselectAll "Diselect all"
        , buttonMdl model 1 SelectAll "Select all"
        , vocabField vocab field
            |> List.map (wordDataItem model)
            |> Lists.ul []
        ]


wordDataItem : Model -> WordData -> Html Msg
wordDataItem model wordData =
    Lists.li []
        [ Lists.content []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.icon
                , Button.colored
                , (Color.text Color.white)
                , (?:) wordData.selected colorSelected colorNotSelected
                , Options.onClick (ToggleSelect wordData.word)
                ]
                [ (?:) wordData.selected (Icon.i "check box") (Icon.i "beach access") ]
            , (?:) model.wordfindDef
                (wordData.def
                    |> List.map (\defItem -> wordDataDef defItem)
                    |> toString
                    |> (++) wordData.word
                    |> text
                )
                (text wordData.word)
            ]
        ]


wordDataDef : Maybe String -> String
wordDataDef def =
    case def of
        Just defStr ->
            defStr

        _ ->
            "No definition"



-- UPDATE


flip : Bool -> Bool
flip b =
    case b of
        True ->
            False

        False ->
            True


(?:) : Bool -> x -> x -> x
(?:) condition a b =
    case condition of
        True ->
            a

        _ ->
            b


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


sortWordFind : Vocab -> Vocab
sortWordFind vocab =
    { vocab | wordfind = (List.sortBy (\vocabT -> vocabT.word) vocab.wordfind) }


formatResponse : WebData Vocab -> WebData Vocab
formatResponse response =
    case response of
        RemoteData.Success _ ->
            RemoteData.map sortWordFind response

        _ ->
            response


addSnackbar : String -> String -> String -> Model -> ( Model, Cmd Msg )
addSnackbar payload message label model =
    let
        ( snackbar_, effect ) =
            Snackbar.add (Snackbar.snackbar payload message label) model.snackbar
    in
        ( { model | snackbar = snackbar_ }, Cmd.map Snackbar effect )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( { model | content = newContent }, Cmd.none )

        Curl ->
            { model | result = RemoteData.Loading }
                |> addSnackbar "Fetch" "Fetching" "Server"
                |> map2nd List.singleton
                |> map2nd ((::) (curl model.content))
                |> map2nd Cmd.batch

        --let
        --    ( model_, snackBarEffect ) =
        --        ({ model | result = RemoteData.Loading })
        --            |> addSnackbar "Fetch" "Fetching" "Server"
        --in
        --    ( model_, Cmd.batch [ snackBarEffect, curl model.content ] )
        ToggleDef ->
            ( { model | wordfindDef = (flip model.wordfindDef) }, Cmd.none )

        ToggleSelect word ->
            remoteDataUpdate model (\vocab -> selectWordData vocab word)

        SelectAll ->
            remoteDataUpdate model (\vocab -> setSelect vocab True)

        DiselectAll ->
            remoteDataUpdate model (\vocab -> setSelect vocab False)

        OnResponse response ->
            ({ model | result = formatResponse <| response })
                |> addSnackbar "Fetch" "Done fetching" "Server"

        ReqSave ->
            ( model
            , (case model.result of
                RemoteData.Success vocab ->
                    save vocab

                _ ->
                    Cmd.none
              )
            )

        SaveResponse response ->
            addSnackbar "Save" "Saved" "Server" model

        SelectTab idx ->
            ( { model | selectedTab = idx }, Cmd.none )

        Snackbar (Snackbar.Begin k) ->
            model |> pure

        Snackbar (Snackbar.End k) ->
            model |> pure

        Snackbar msg_ ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- COMMANDS


curl : String -> Cmd Msg
curl query =
    Http.get ("http://localhost:4000/lookupword?w=" ++ query) (vocabDecoder query)
        |> RemoteData.sendRequest
        |> Cmd.map OnResponse


save : Vocab -> Cmd Msg
save vocab =
    Http.post "http://localhost:4000/save" (Http.jsonBody (encodeVocab vocab)) Decode.string
        |> RemoteData.sendRequest
        |> Cmd.map SaveResponse



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Routing.RouteUrlProgram Never Model Msg
main =
    Routing.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
