port module Types exposing (..)

import Json.Encode as JE

type alias SocketSpec = (String, List String)

type alias Socket = { url : String
                    , fd : Int
                    }

type Msg = OpenSocket String
         | SendSocket
         | SocketOpened Socket
         | SocketNotOpened
         | SocketReceived JE.Value
         | InputUpdate String

type alias ChatData = { msg : String
                      }
