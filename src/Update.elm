module Update exposing (..)

import Main exposing (Msg(..))
import Main exposing (Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )