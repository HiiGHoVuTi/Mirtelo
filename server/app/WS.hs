{-# LANGUAGE OverloadedStrings #-}

module WS (
  run_server
          ) where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn $ "Sending: " <> message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

cast :: Text -> WS.Connection -> IO ()
cast message conn = do
  WS.sendTextData conn message

run_server :: IO ()
run_server = do
  state <- newMVar newServerState
  WS.runServer "localhost" 8000 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return()) $ do
    msg <- WS.receiveData conn
    clients <- readMVar state
    case msg of
      -- error cases
      _| not (prefix `T.isPrefixOf` msg) ->
         WS.sendTextData conn ("Wrong announcement" :: Text)
       | clientExists client clients ->
         WS.sendTextData conn ("User already exists" :: Text)
      -- accepted, setup the function on disconnect
       | otherwise -> flip finally disconnect $ do
          modifyMVar_ state $ \s -> do
            let s' = addClient client s
            -- user connected
            return s'
          talk client state
        where
          prefix = ""
          client = (T.drop (T.length prefix) msg, conn)
          disconnect = do
            s <- modifyMVar state $ \s ->
              let s' = removeClient client s in return (s', s')
            -- user disconnected
            return ()

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  msg <- WS.receiveData conn
  T.putStrLn $ "Received: " <> msg
  readMVar state >>= broadcast (msg)
  return ()

