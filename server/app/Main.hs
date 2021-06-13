{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import WS (run_server)


import Database.MongoDB
import qualified Data.Text as T

main = do
  let db = "test"
  pipe <- connect (host "127.0.0.1")

  run_server

  putStrLn "Server is now up !"

  close pipe
  where
    run pipe = access pipe master "database"

