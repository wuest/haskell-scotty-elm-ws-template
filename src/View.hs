{-# LANGUAGE OverloadedStrings #-}

module View ( index )
where

import Prelude

import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

-- Partials

headerNormal :: Html
headerNormal = H.head $ do
    H.link ! A.rel "stylesheet" ! A.href "/main.css"
    H.script ! A.src "/websocket.js" $ ""
    H.script ! A.src "/main.js" $ ""

-- Public views

index :: Html
index = html $ do
    headerNormal
    H.body $ H.div ! A.id "app" $ ""
    H.script $ toHtml $ unlines [ "app = Elm.Main.init({node : document.getElementById('app'), flags : websocket_uri});"
                                , "ElmWS.init(app);"
                                ]
