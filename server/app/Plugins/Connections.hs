
module Plugins.Connections (
  Event (..),
  on
                   ) where

import DB
import WebClasses


data Event = Connect | Disconnect
on :: Event -> Client -> ServerState -> PipeRun -> IO ()


-- on socket connection
on Connect    client state run = do return()



-- on socket disconnection
on Disconnect client state run = do return()

