{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Lib     (LRUCache, LRUValue, getFromCache, insertIntoCache,
                          makeSizedLRU)
import           Prelude (IO, String, print)

main :: IO ()
main = do
  let myCache = makeSizedLRU 10 :: LRUCache String String
  print "I have a cache!"
