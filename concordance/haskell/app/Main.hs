{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Lib
import Control.Monad (mapM_)
import Data.List (sort)
import Prelude (IO, FilePath, return, Int, reverse, length, ($), (==), (/=), filter, take, uncurry)
import GHC.IO.Handle (hClose)
-- import GHC.Types (([](..)))
import System.IO (openFile, IOMode(..))
import Data.Text (Text, words)
import Data.Text.IO (getContents, hGetContents)
import Text.Printf (printf)

readContents :: FilePath -> IO Text
readContents "" = getContents
readContents fn = do
  handle <- openFile fn ReadMode
  lines <- hGetContents handle
  hClose handle
  return lines

sortFunctionOne :: [Text] -> [(Int, Text)]
sortFunctionOne [] = []
sortFunctionOne (x:xs) = do
  let count = length $ filter (==x) (x:xs)
      nextList = filter (/=x) xs
  (count, x) : sortFunctionOne nextList

main :: IO ()
main = do
  input <- readContents ""
  let processedInput = sort $ words input
      wordCounts = sortFunctionOne processedInput
      lastNWords = reverse $ take 10 $ reverse wordCounts
  mapM_ (uncurry $ printf "%7d %s") lastNWords

