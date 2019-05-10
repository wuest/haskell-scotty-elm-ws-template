module Websockets ( initState
                  , app
                  , broadcast )
where

import Prelude
import Control.Monad          ( forever )

import qualified Control.Concurrent             as Concurrent
import qualified Control.Concurrent.STM         as STM
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text

import qualified Network.WebSockets             as WS

type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

broadcast :: Concurrent.MVar State -> STM.TChan Text.Text -> IO loop
broadcast stateRef broadcastChan = do
    chan <- STM.atomically $ STM.dupTChan broadcastChan
    forever $ do
        msg <- STM.atomically $ STM.readTChan chan
        sendFrom (negate 1) stateRef msg

nextId :: State -> ClientId
nextId = Maybe.maybe 0 (1 +) . maxM . List.map fst

maxM :: Ord a => [a] -> Maybe a
maxM [] = Nothing
maxM xs = Just $ maximum xs

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
    let clientId = nextId state
    return ((clientId, conn) : state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
    return $ withoutClient clientId state

sendFrom :: ClientId -> Concurrent.MVar State -> Text.Text -> IO ()
sendFrom clientId stateRef msg = do
    clients <- Concurrent.readMVar stateRef
    let otherClients = withoutClient clientId clients
    Monad.forM_ otherClients $ \(_, conn) ->
        WS.sendTextData conn msg

startApp :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
startApp conn clientId stateRef = Monad.forever $
      WS.receiveData conn >>= sendFrom clientId stateRef

app :: Concurrent.MVar State -> WS.PendingConnection -> IO ()
app stateRef pendingConn = do
    conn <- WS.acceptRequest pendingConn
    clientId <- connectClient conn stateRef
    WS.forkPingThread conn 1
    Exception.finally
        (startApp conn clientId stateRef)
        (disconnectClient clientId stateRef)

initState :: IO (Concurrent.MVar State)
initState = Concurrent.newMVar []
