{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (mapM_)
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Printf (printf)
import Control.Arrow (second)

-- Thanks Yorgey
transMap :: (Ord k, Ord a) => M.Map k a -> M.Map a (S.Set k)
transMap = M.fromListWith S.union . map (second S.singleton . swap) . M.toList
  where swap (one, two) = (two, one)

countWords wordList =
  L.foldl' f M.empty wordList
  where f wordMap word =
          updateWordMap wordMap word
        updateWordMap wordMap word =
          M.insertWith (+) word 1 wordMap

main :: IO ()
main = do
  input <- B.getContents
  let text = TE.decodeUtf8 input
      wordList = T.words text
      wordCounts = countWords wordList
      topWords = take 10 $ M.toDescList $ transMap wordCounts
  print topWords
