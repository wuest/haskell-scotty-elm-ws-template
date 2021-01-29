{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Data.Database where

import Prelude

import Data.Model

import Control.Monad.IO.Class     ( liftIO )
import Control.Monad.Trans.Reader ( ReaderT )
import Control.Monad.Reader       ( asks )
import Control.Monad.Logger       ( runStdoutLoggingT )

import qualified Data.ByteString.Char8 as C8 ( pack )
import qualified Data.Text             as T  ( pack )

import Database.Persist.Sql        ( SqlBackend, ConnectionPool, runMigration, runSqlPool )

-- Individual drivers can be commented out to remove support
import Database.Persist.MySQL      ( ConnectInfo(..), defaultConnectInfo, createMySQLPool)
import Database.Persist.Postgresql ( createPostgresqlPool,)
import Database.Persist.Sqlite     ( createSqlitePool )

newtype DBPool = DBPool { getPool :: ConnectionPool }
type DBPoolM   = ReaderT DBPool IO
type Query m   = ReaderT SqlBackend IO m

type DBName = String
type DBUser = String
type DBHost = String
type DBPassword = String
type DBPort = Int

connectSqlite :: DBName -> IO DBPool
connectSqlite name = do
    let n = T.pack $ name
    pool <- runStdoutLoggingT $ createSqlitePool n 10
    return $ DBPool pool

connectMySQL :: DBName -> DBUser -> DBHost -> DBPassword -> DBPort -> IO DBPool
connectMySQL name user pass host port = do
    let info = defaultConnectInfo { connectDatabase = name
                                  , connectUser     = user
                                  , connectPassword = pass
                                  , connectHost     = host
                                  , connectPort     = fromIntegral port
                                  }
    pool <- runStdoutLoggingT $ createMySQLPool info 10
    return $ DBPool pool

connectPostgres :: DBName -> DBUser -> DBHost -> DBPassword -> DBPort -> IO DBPool
connectPostgres name user host pass port = do
    let h = host
        o = show port
        u = user
        p = pass
        n = name
        conn = C8.pack $ "host=" ++ h ++ " dbname=" ++ n ++ " port=" ++ o ++ " user=" ++ u ++ " password=" ++ p
    pool <- runStdoutLoggingT $ createPostgresqlPool conn 10
    return $ DBPool pool

run :: Query m -> DBPoolM m
run query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

initialize :: DBPoolM ()
initialize = run $ runMigration migrateAll
