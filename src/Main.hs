{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import Control.Monad ( unless )
import Control.Monad.Reader ( runReaderT )

import Web.Scotty.Trans
import Network.Wai.Middleware.Static ( addBase, hasPrefix, noDots, staticPolicy, (>->) )
import Network.Wai.Handler.WebSockets ( websocketsOr )
import Network.WebSockets ( defaultConnectionOptions )

import qualified System.Directory as Dir

import qualified Opts
import qualified Routes
import qualified Websockets as WS

start :: Int -> FilePath -> IO ()
start port base = do
    appState <- WS.initState
    scottyT port (`runReaderT` base) $ do
        middleware $ websocketsOr defaultConnectionOptions $ WS.app appState
        middleware $ staticPolicy $ noDots >-> hasPrefix "static" >-> addBase base
        Routes.routes

setupDataDir :: FilePath -> IO ()
setupDataDir path = do
    exists <- Dir.doesDirectoryExist path
    unless exists $ do
        putStrLn $ "Data directory (" ++ path ++ ") doesn't exist - creating..."
        Dir.createDirectory path

    exists' <- Dir.doesDirectoryExist (path ++ "/static")
    unless exists' $ do
        putStrLn $ "Data directory (" ++ path ++ "/static) doesn't exist - creating..."
        Dir.createDirectory (path ++ "/static")

main :: IO ()
main = do
    opts <- Opts.getOpts
    let port = Opts.webPort opts
        base = Opts.baseDir opts
    _ <- setupDataDir base
    putStrLn $ "Starting service on port " ++ show port
    start port base
