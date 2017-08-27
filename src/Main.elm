module Main exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import String
import Html exposing (..)
import Html.Attributes exposing (placeholder, href)
import Html.Events exposing (onInput, onClick)
import RemoteData exposing (WebData)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Tabs as Tabs
import Material.Options as Options exposing (css)
import Navigation
import RouteUrl as Routing


--see https://github.com/debois/elm-mdl/blob/master/demo/Demo.elm
--see https://github.com/debois/elm-mdl/blob/master/demo/Demo/Tabs.elm
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
    , mdl : Material.Model
    , selectedTab : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { content = ""
      , wordfindDef = True
      , result = RemoteData.NotAsked
      , mdl = Material.model
      , selectedTab = 1
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
    | Mdl (Material.Msg Msg)
    | SelectTab Int



-- ROUTING


tabs : List ( String, String )
tabs =
    [ ( "Contains", "contains" )
    , ( "Family", "family" )
    , ( "Synonyms", "synonyms" )
    , ( "Antonyms", "antonyms" )
    ]


tabUrls : Array String
tabUrls =
    List.map (\( _, x ) -> x) tabs |> Array.fromList


urlTabs : Dict String Int
urlTabs =
    List.indexedMap (\idx ( _, x ) -> ( x, idx )) tabs |> Dict.fromList


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
--highlightNav : Route -> Model -> string
--highlightNav route model =


nav : Model -> Html Msg
nav model =
    Tabs.render Mdl
        [ 0 ]
        model.mdl
        [ Tabs.ripple
        , Tabs.onSelectTab SelectTab
        , Tabs.activeTab model.selectedTab
        ]
        [ Tabs.label []
            [ text "Contain" ]
        , Tabs.label []
            [ text "Family" ]
        , Tabs.label []
            [ text "Synonyms" ]
        , Tabs.label []
            [ text "Antonyms" ]
        ]
        [--Options.div
         --    [ css "margin" "24px auto"
         --    , css "align-items" "flex-start"
         --    , Options.center
         --    , css "overflow-y" "auto"
         --    , css "height" "512px"
         --    ]
         --    [ case model.tab of
         --        0 ->
         --            aboutTab
         --        _ ->
         --            exampleTab
         --    ]
        ]



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
        , Button.render Mdl
            [ 1 ]
            model.mdl
            [ Options.onClick Curl

            --, css "margin" "0 24px"
            ]
            [ text "Look!" ]
        , Button.render Mdl
            [ 2 ]
            model.mdl
            [ Options.onClick ToggleDef ]
            [ text "Definition" ]
        , nav model
        , div [] [ maybeResult model model.result ]
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
            text "Loaded"

        RemoteData.Failure error ->
            text (toString error)


containView : Bool -> Vocab -> Html Msg
containView showDef vocab =
    div []
        [ countHeader vocab.wordfind "wordfind"
        , vocab.wordfind
            |> List.map (wordDataRow showDef)
            |> vocabTable
        ]


familyView : Bool -> Vocab -> Html Msg
familyView showDef vocab =
    div []
        [ countHeader vocab.cambridge "cambridge"
        , vocab.cambridge
            |> List.map (wordDataRow showDef)
            |> vocabTable
        ]


synonymsView : Bool -> Vocab -> Html Msg
synonymsView showDef vocab =
    div []
        [ countHeader vocab.synonyms "synonyms"
        , vocab.synonyms
            |> List.map (wordDataRow False)
            |> vocabTable
        ]


antonymsView : Bool -> Vocab -> Html Msg
antonymsView showDef vocab =
    div []
        [ countHeader vocab.antonyms "antonyms"
        , vocab.antonyms
            |> List.map (wordDataRow False)
            |> vocabTable
        ]


countHeader : List a -> String -> Html Msg
countHeader theList src =
    h5 []
        [ theList
            |> List.length
            |> toString
            |> (++) ("Found " ++ src ++ " ")
            |> text
        ]



--NotFoundRoute ->
--    [ text "Route not found"
--    ]


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

        Mdl msg_ ->
            Material.update Mdl msg_ model

        SelectTab idx ->
            ( { model | selectedTab = idx }, Cmd.none )



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
