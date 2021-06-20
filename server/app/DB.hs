
module DB (
  PipeRun
          ) where

{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
import Database.MongoDB
import qualified Data.Text as T

type PipeRun = (Action IO [Document] -> IO [Document])


