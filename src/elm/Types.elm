module Types exposing (..)

import Json.Encode as JE

type alias SocketSpec = (String, List String)

type alias Socket = { url : String
                    , fd : Int
                    }

type Msg = OpenSocket String
         | SendSocket
         | SocketOpened Socket
         | SocketReopened Socket
         | SocketNotOpened
         | SocketError (Maybe Int)
         | SocketReceived JE.Value
         | InputUpdate String

type alias ChatData = { msg : String
                      }
