{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Data.Mysql ( connect, run, initialize )
    where

import Prelude

import Control.Monad.IO.Class     ( liftIO )
import Control.Monad.Trans.Reader ( ReaderT )
import Control.Monad.Reader       ( asks )
import Control.Monad.Logger       ( runStdoutLoggingT )

import Database.Persist.MySQL     ( ConnectInfo (..), SqlBackend, ConnectionPool
                                  , defaultConnectInfo, createMySQLPool, runSqlPool
                                  , runMigration
                                  )

import Data.Model

import qualified Opts as O

newtype DBPool = DBPool { getPool :: ConnectionPool }
type DBPoolM = ReaderT DBPool IO

type Query = ReaderT SqlBackend IO ()

connect :: O.Options -> IO DBPool
connect opts = do
    let info = defaultConnectInfo { connectDatabase = O.dbName opts
                                  , connectUser     = O.dbUser opts
                                  , connectPassword = O.dbPass opts
                                  , connectHost     = O.dbHost opts
                                  , connectPort     = fromIntegral $ O.dbPort opts
                                  }
    pool <- runStdoutLoggingT $ createMySQLPool info 10
    return $ DBPool pool

run :: Query -> DBPoolM ()
run query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

initialize :: DBPoolM ()
initialize = run $ runMigration migrateAll
