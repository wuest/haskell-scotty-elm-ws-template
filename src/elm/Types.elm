module Types exposing (..)

import Json.Encode as JE

import Websocket as WS

type Msg = OpenSocket String
         | SendSocket
         | Websocket WS.Msg
         | InputUpdate String

type alias ChatData = { msg : String }

type alias SocketSpec = (String, List String)

