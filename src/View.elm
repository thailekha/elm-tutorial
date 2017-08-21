module View exposing (..)

import Html exposing (Html, div, text)
import Main exposing (Msg)
import Main exposing (Model)

view : Model -> Html Msg
view model =
    div []
        [ text model ]