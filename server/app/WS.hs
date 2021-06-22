{-# LANGUAGE OverloadedStrings #-}

module WS (
  run_server,

  Client,
  ServerState
          ) where

import Prelude hiding (lookup)

import DB
import WebClasses
import qualified Plugins.Connections as Connections

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
import qualified Data.ByteString as BS
import TextShow
import Database.MongoDB

import qualified Network.WebSockets as WS

import qualified Data.Aeson as A
import qualified Data.AesonBson as AB

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

broadcast_text :: Text -> ServerState -> IO ()
broadcast_text message clients = do
  T.putStrLn $ "Sending: " <> message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message


broadcast_BSON :: Document -> ServerState -> IO ()
broadcast_BSON docs = let
      text = bson_to_text docs
  in  broadcast_text text

cast :: Text -> WS.Connection -> IO ()
cast message conn = do
  WS.sendTextData conn message

run_server :: Pipe -> IO ()
run_server pipe = do
  state <- newMVar newServerState
  WS.runServer "localhost" 8000 $ application state pipe

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
            Connections.on Connections.Connect client s' pipe
            return s'
          talk client state pipe
        where
          prefix = ""
          client = (T.drop (T.length prefix) msg, conn)
          disconnect = do
            s <- modifyMVar state $ \s ->
              let s' = removeClient client s in return (s', s')
              -- user disconnected
            Connections.on Connections.Disconnect client s pipe
            return ()

-- decode_client_message :: ... -> Document
decode_client_message = ( A.decode !?> AB.bsonify nul )
  where nul = \_-> Float 0

talk client@(user, conn) state pipe = forever $ do
  msg <- WS.receiveData conn
  -- T.putStrLn $ "Received: " <> msg
  -- db_response <- run all_people
  -- readMVar state >>= broadcast_BSON sample_message -- db_response
  doc <- return $ decode_client_message msg
  react client pipe state doc
  -- readMVar state >>= broadcast_BSON doc
  return ()


react client@(user, conn) pipe state doc = case doc `decode` "intent" of
  Just "\"message\"" -> do
    -- create new message based on the content here
    let
      inner    = "contents" `lookup` doc <?> []

      contents = inner `decode` "contents"
      thread   = inner `decode` "thread"
      author   = ["_id" =: user]

      msg      = networkMessage (contents <?>"") (thread <?>"") author

    run pipe $ saveMessage $ ("contents" `lookup` msg) <?> []

    readMVar state >>= broadcast_BSON msg
  Just "\"recent messages\"" -> do
    let
      inner  = "contents" `lookup` doc <?> []
      thread = inner `decode` "thread"

      msgs   = run pipe $ recent_messages $ thread <?> ""

    msg    <- recentMessages <$> msgs

    readMVar state >>= broadcast_BSON msg
  otherwise         -> do
    WS.sendTextData conn ("invalid intent." :: Text)
  where
    decode x k = (bsonValue_to_text <$> k `look` x) :: Maybe Text


all_people :: Action IO [Document]
all_people = rest =<< find (select [] "people") {sort = []}

recent_messages :: Text -> Action IO [Document]
recent_messages thread = take 50 <$> (rest =<< find (select [("thread"::Text) =: ("\"general>main\""::Text)] "message") {sort = []})

sample_message :: Document
sample_message = networkMessage "717064" "156460" ["_id" =: ("541402" :: Text)]

printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . exclude ["_id"]) docs
