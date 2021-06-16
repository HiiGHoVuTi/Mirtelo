{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import WS (run_server)


import Database.MongoDB
import qualified Data.Text as T

main = do
  pipe <- connect (host "127.0.0.1")

  putStrLn "Server is now up !"

  run pipe $ do
    delete $ select [] "people"
    insert "people" ["name" =: "Joe", "last" =: "M"]

  run_server $ run pipe

  close pipe
  where
    run pipe = access pipe master "database"
