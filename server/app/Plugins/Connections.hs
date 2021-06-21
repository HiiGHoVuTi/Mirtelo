
{-# LANGUAGE OverloadedStrings #-}

module Plugins.Connections (
  Event (..),
  on
                   ) where

import DB
import WebClasses
import qualified Data.Text as T

data Event = Connect | Disconnect | Message
on :: Event -> Client -> ServerState -> PipeRun -> IO ()


-- on socket connection
on Connect    client state run = do return()



-- on socket disconnection
on Disconnect client state run = do return()

