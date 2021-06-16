{-# LANGUAGE OverloadedStrings #-}

module WS (
  run_server
          ) where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import TextShow
import Database.MongoDB

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

run_server :: (Action IO [Document] -> IO [Document]) -> IO ()
run_server pipe = do
  state <- newMVar newServerState
  WS.runServer "localhost" 8000 $ application state pipe

application :: MVar ServerState -> (Action IO [Document] -> IO [Document]) -> WS.ServerApp
application state pipe pending = do
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
          talk client state pipe
        where
          prefix = ""
          client = (T.drop (T.length prefix) msg, conn)
          disconnect = do
            s <- modifyMVar state $ \s ->
              let s' = removeClient client s in return (s', s')
            -- user disconnected
            return ()

talk :: Client -> MVar ServerState -> (Action IO [Document] -> IO [Document]) -> IO ()
talk (user, conn) state run = forever $ do
  msg <- WS.receiveData conn
  T.putStrLn $ "Received: " <> msg
  run $ do
    all_people
  db_response <- run all_people
  readMVar state >>= broadcast (showt $ show db_response :: Text)
  return ()

all_people :: Action IO [Document]
all_people = rest =<< find (select [] "people") {sort = []}
