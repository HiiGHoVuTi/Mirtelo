
{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck

import DB
import WebClasses
import Database.MongoDB
import Data.Text
import TextShow

default_works :: Eq a => Maybe a -> a -> Bool
default_works val def = case val of
    Nothing  -> result == def
    (Just x) -> result == x
  where
    result = val <?> def


b2t_works :: String -> Bool
b2t_works s = let
    t      = showt s
    result = bson_to_text [t =: [t =: t]]
  in result == "{"<>t<>":"<>  "{"<>t<>":"<>t<>"}" <>"}" || isInfixOf "\"" t


main :: IO ()
main = do

  -- default block
  quickCheck $ (\d -> default_works d (0 :: Int)) -- default for elements
  quickCheck $ (\d -> default_works d ([] :: [Char])) -- default for lists

  -- conversions block
  quickCheck $ (\s -> b2t_works s)
