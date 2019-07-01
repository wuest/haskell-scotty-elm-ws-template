{-# LANGUAGE OverloadedStrings #-}

module Routes ( routes )
where

import Prelude
import Data.Text.Lazy ( Text )
import Control.Monad.Reader ( ReaderT )

import Web.Scotty.Trans
import Text.Blaze.Html5 ( Html )
import Text.Blaze.Html.Renderer.Text

import qualified Const
import qualified View

type Router = ScottyT Text (ReaderT FilePath IO) ()
type Response = ActionT Text (ReaderT FilePath IO) ()

blaze :: Html -> Response
blaze = html . renderHtml

mainCSS :: Response
mainCSS = do
    setHeader "Content-Type" "text/css"
    text Const.mainCSS

mainJS :: Response
mainJS = do
    setHeader "Content-Type" "application/javascript"
    text Const.mainJS

websocketJS :: Response
websocketJS = do
    setHeader "Content-Type" "application/javascript"
    text Const.websocketJS

index :: Response
index = blaze View.index

routes :: Router
routes = do
-- Health Check
    get "/_ping" $ text "OK"

-- Static Content
    get "/main.css" mainCSS
    get "/main.js" mainJS
    get "/websocket.js" websocketJS

-- Public routes
    get "/" index
