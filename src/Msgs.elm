module Msgs exposing (..)

import Models exposing (Player)
import RemoteData exposing (WebData)

-- OnFetchPlayers will be called when we get the response from the server. This message will carry a WebData (List Player)

type Msg
    = OnFetchPlayers (WebData (List Player))