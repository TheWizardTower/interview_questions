{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Data.Foldable (foldl')
import           Lib           (LRUCache, LRUValue, getFromCache,
                                insertIntoCache, makeSizedLRU)
import           Prelude       (IO, String, print)

main :: IO ()
main = do
  let myCache = makeSizedLRU 5 :: LRUCache String String
      mapVals = [("Adam", "McCullough"), ("Daggerfall", "Fun Dungeon Torture"), ("Hollow Knight", "Metroidvania"), ("ARMA 3", "Fun Milsim Torture"), ("EUIV", "History Nerd Candy"), ("Adam", "Overwrite-test"), ("Neverwinter Nights", "NERRRRRRDS") ]
      myCache' = foldl' (\cache (key, val) -> insertIntoCache cache key val) myCache mapVals
  print "I have a cache!"
  print myCache
  print myCache'
