port module Websocket exposing ( open , openWithProto, openWithProtos
                               , createWS, send
                               , subscriptions
                               )

import Json.Encode as JE
import Json.Decode as JD

import Types exposing (Socket, SocketSpec, Msg(..))

type alias SocketData = { socket : Socket
                        , data : JE.Value
                        }

port createWS : SocketSpec -> Cmd msg
port sendWS : SocketData -> Cmd msg

port newFD : (JD.Value -> msg) -> Sub msg
port recv : (JE.Value -> msg) -> Sub msg
port error : (JD.Value -> msg) -> Sub msg
port reopenedFD : (JD.Value -> msg) -> Sub msg

open : String -> Cmd msg
open url = createWS (url, [])

openWithProto : String -> String -> Cmd msg
openWithProto url proto = createWS (url, [proto])

openWithProtos : String -> List String -> Cmd msg
openWithProtos url protos = createWS (url, protos)

send : Socket -> JE.Value -> Cmd msg
send s v = sendWS { socket = s, data = v }

decodeSocket : JD.Value -> Result JD.Error Socket
decodeSocket = JD.decodeValue <| JD.map2 Socket
    (JD.field "url" JD.string)
    (JD.field "fd" JD.int)

decodeSocketError : JD.Value -> Result JD.Error Int
decodeSocketError = JD.decodeValue <| (JD.field "fd" JD.int)

processNewFD : JD.Value -> Msg
processNewFD value = case (decodeSocket value) of
    Ok socket -> SocketOpened socket
    Err _     -> SocketNotOpened

processReopenedFD : JD.Value -> Msg
processReopenedFD value = case (decodeSocket value) of
    Ok socket -> SocketReopened socket
    Err _     -> SocketNotOpened

processSocketError : JD.Value -> Msg
processSocketError value = case (decodeSocketError value) of
    Ok socket -> SocketError <| Just socket
    Err _     -> SocketError Nothing

subscriptions : Sub Msg
subscriptions = Sub.batch [ newFD processNewFD
                          , recv SocketReceived
                          , error processSocketError
                          , reopenedFD processReopenedFD
                          ]
