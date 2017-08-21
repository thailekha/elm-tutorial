module View exposing (..)

import Html exposing (Html, div, text)
import Msgs exposing (Msg)
import Models exposing (Model)
import Players.List


view : Model -> Html Msg
view model =
    div []
        [ page model ]


-- Html Msg means that this Html element would produce messages tagged with Msg. We will see this when we introduce some interaction

page : Model -> Html Msg
page model =
    Players.List.view model.players