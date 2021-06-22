{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import WS (run_server)
import DB (run)

import Database.MongoDB
import qualified Data.Text as T

main = do
  pipe <- connect (host "127.0.0.1")

  putStrLn "Server is now up !"

  run pipe $ do
    -- delete $ select [] "message"
    -- insert "message" ["author" =: "Maxime", "contents" =: "Welcome !", "thread" =: "\"general>main\""]
    return()

  run_server pipe

  close pipe
