{-# LANGUAGE TemplateHaskell #-}

module Const where

import Prelude ( ($), String )
import Data.Text.Lazy ( fromStrict )
import Data.Text.Internal.Lazy ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.FileEmbed  ( embedFile )

applicationName :: String
applicationName = "scotty-elm-ws-template"

version :: String
version = "0.1.0.0"

mainCSS :: Text
mainCSS = fromStrict $ decodeUtf8 $(embedFile "static/main.css")

mainJS :: Text
mainJS = fromStrict $ decodeUtf8 $(embedFile "static/main.js")

websocketJS :: Text
websocketJS = fromStrict $ decodeUtf8 $(embedFile "static/websocket.js")
