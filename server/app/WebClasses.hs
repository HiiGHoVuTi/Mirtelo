
module WebClasses (
  Client,
  ServerState
                  ) where

import Data.Text as T
import qualified Network.WebSockets as WS


type Client = (Text, WS.Connection)
type ServerState = [Client]


