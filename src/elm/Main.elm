port module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as JE
import Json.Decode as JD

import Types exposing (Msg(..), Socket, ChatData)
import Websocket as WS

main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

{- MODEL -}

type alias Model = { socket : Maybe Socket
                   , username : String
                   , input : String
                   , chatlog : String
                   }

type alias ChatMsg = { msg : String }

init : String -> (Model, Cmd Msg)
init url = ( { socket = Nothing
             , username = ""
             , input = ""
             , chatlog = ""
             }
           , WS.open url
           )

send : Maybe Socket -> JE.Value -> Cmd msg
send ms v = case ms of
    Nothing -> Cmd.none
    Just s -> WS.send s v

{- UPDATE -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OpenSocket url ->
            (model, WS.open url)
        SendSocket ->
            let data = JE.object [("msg", JE.string model.input)] in
            ({ model | input = "" }, send model.socket data)
        SocketOpened newsocket ->
            ({ model | socket = Just newsocket }, Cmd.none)
        SocketNotOpened ->
            (model, Cmd.none)
        SocketReceived data ->
            ({ model | chatlog = updateChatlog model.chatlog data }, Cmd.none)
        InputUpdate text ->
            ({ model | input = text }, Cmd.none)

decodeChatlog : JE.Value -> Result JD.Error ChatMsg
decodeChatlog = JD.decodeValue <| JD.map ChatMsg (JD.at ["data", "msg"] JD.string)

updateChatlog : String -> JE.Value -> String
updateChatlog log new =
  case (decodeChatlog new) of
    Ok m   -> log ++ "\n" ++ m.msg
    Err _  -> log

{- SUBSCRIPTIONS -}
subscriptions : Model -> Sub Msg
subscriptions _ = WS.subscriptions

{- VIEW -}
li : String -> Html Msg
li string = Html.li [] [Html.text string]

view : Model -> Html Msg
view model = Html.div []
    --[ Html.form [HE.onSubmit (WebsocketIn model.input)] -- Short circuit to test without ports
    [ Html.span [HA.id "chatlog", HA.style "overflow" "scroll", HA.style "height" "800", HA.style "width" "600"] [Html.text model.chatlog]
    , Html.form [HE.onSubmit SendSocket]
        [ Html.input [HA.placeholder "Enter some text.", HA.value model.input, HE.onInput InputUpdate] []
        , Html.button [] [Html.text "submit"]
        ]
    ]
