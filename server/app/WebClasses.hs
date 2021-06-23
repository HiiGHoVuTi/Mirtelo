
{-# LANGUAGE OverloadedStrings #-}

module WebClasses (
  Client,
  ServerState,

  bson_to_text, bsonValue_to_text,

  (!?>), (<?>)
                  ) where

import qualified Data.Text as T
import Data.Text (Text)
import TextShow

import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)


import qualified Network.WebSockets as WS

import Database.MongoDB

import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.AesonBson as AB

type Client = (Text, WS.Connection)
type ServerState = [Client]


bson_to_text :: Document -> Text
bson_to_text docs = let
      aeson = AB.aesonify docs
      json  = A.encodeToTextBuilder aeson
  in  T.replace "\\\\" "\\" . T.replace "\\\"" "" . toStrict . toLazyText $ json

bsonValue_to_text :: Value -> Text
bsonValue_to_text docs = let
      aeson = AB.aesonifyValue docs
      json  = A.encodeToTextBuilder aeson
  in  T.replace "\\\\" "\\" . T.replace "\\\"" "" . toStrict . toLazyText $ json


-- Don't mind this mess, or fix if you know the proper haskell built-ins
(??>) :: (a -> Maybe b) -> (b -> c) -> (a -> Maybe c)
(??>) f g = g' . f
  where
    g' Nothing  = Nothing
    g' (Just x) = Just (g x)

(!?>) :: (a -> Maybe b) -> (b -> Document) -> a -> Document
(!?>) f g x = (f ??> g $ x) <?> []

(<?>) :: Maybe a -> a -> a
(<?>) (Just val) _ = val
(<?>) _ val        = val

