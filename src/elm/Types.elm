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

type alias SocketData = { socket : Socket
                        , data : JE.Value
                        }

type alias SocketConfig = { protocols : List String
                          , autoReconnect : Bool
                          , reconnectWait : Float
                          , reconnectBackoffMultiplier : Float
                          , reconnectBackoffMaxWait : Maybe Float
                          , reconnectMaxTries : Maybe Int
                          }

type alias ChatData = { msg : String
                      }
