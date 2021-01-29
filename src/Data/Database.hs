{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Data.Database where

import Prelude

import Data.Model
import qualified Opts as O

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

connectSqlite :: O.Options -> IO DBPool
connectSqlite opts = do
    let n = T.pack $ O.dbName opts
    pool <- runStdoutLoggingT $ createSqlitePool n 10
    return $ DBPool pool

connectMySQL :: O.Options -> IO DBPool
connectMySQL opts = do
    let info = defaultConnectInfo { connectDatabase = O.dbName opts
                                  , connectUser     = O.dbUser opts
                                  , connectPassword = O.dbPass opts
                                  , connectHost     = O.dbHost opts
                                  , connectPort     = fromIntegral $ O.dbPort opts
                                  }
    pool <- runStdoutLoggingT $ createMySQLPool info 10
    return $ DBPool pool

connectPostgres :: O.Options -> IO DBPool
connectPostgres opts = do
    let h = O.dbHost opts
        o = show $ O.dbPort opts
        u = O.dbUser opts
        p = O.dbPass opts
        n = O.dbName opts
        conn = C8.pack $ "host=" ++ h ++ " dbname=" ++ n ++ " port=" ++ o ++ " user=" ++ u ++ " password=" ++ p
    pool <- runStdoutLoggingT $ createPostgresqlPool conn 10
    return $ DBPool pool

run :: Query m -> DBPoolM m
run query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

initialize :: DBPoolM ()
initialize = run $ runMigration migrateAll
