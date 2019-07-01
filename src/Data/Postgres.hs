{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Data.Postgres ( connect, run, initialize )
    where

import Prelude

import Data.ByteString.Char8       ( pack )
import Control.Monad.IO.Class      ( liftIO )
import Control.Monad.Trans.Reader  ( ReaderT )
import Control.Monad.Reader        ( asks )
import Control.Monad.Logger        ( runStdoutLoggingT )
import Database.Persist.Postgresql ( SqlBackend, ConnectionPool
                                   , createPostgresqlPool, runSqlPool
                                   , runMigration )

import Data.Model

import qualified Opts as O

newtype DBPool = DBPool { getPool :: ConnectionPool }
type DBPoolM = ReaderT DBPool IO

type Query = ReaderT SqlBackend IO ()

connect :: O.Options -> IO DBPool
connect opts = do
    let h = O.dbHost opts
        o = show $ O.dbPort opts
        u = O.dbUser opts
        p = O.dbPass opts
        n = O.dbName opts
        conn = pack $ "host=" ++ h ++ " dbname=" ++ n ++ " port=" ++ o ++ " user=" ++ u ++ " password=" ++ p
    pool <- runStdoutLoggingT $ createPostgresqlPool conn 10
    return $ DBPool pool

run :: Query -> DBPoolM ()
run query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

initialize :: DBPoolM ()
initialize = run $ runMigration migrateAll
