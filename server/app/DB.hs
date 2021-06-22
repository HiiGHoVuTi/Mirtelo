
{-# LANGUAGE OverloadedStrings #-}


module DB (

  run,

  networkMessage,
  recentMessages,


  saveMessage,
          ) where


import Prelude hiding (lookup)

import WebClasses

import Database.MongoDB

import Data.Text (Text)
import qualified Data.Text as T
import TextShow

import qualified Data.Aeson as A
import qualified Data.AesonBson as AB



run pipe = access pipe master "database"


-- utils

gid :: Document -> Text
gid doc = case "_id" `lookup` doc of -- this line is evil, it can error
  Just t  -> t
  Nothing -> "what the hell happened here"


-- useful functions

message :: Text -> Text -> Document -> Document
message contents thread author = ["author" =: gid author, "contents" =: contents, "thread" =: thread]

networkMessage :: Text -> Text -> Document -> Document
networkMessage c t a = packIntent "message" $ message c t a

saveMessage :: Document -> Action IO Value
saveMessage doc = insert "message" doc

recentMessages :: [Document] -> Document
recentMessages msgs = packIntent "recent messages" ["messages" =: msgs]


packIntent :: Text -> Document -> Document
packIntent i d = ["intent" =: i, "contents" =: d]


