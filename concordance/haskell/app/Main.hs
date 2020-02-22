{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Control.Arrow      (second)
import           Control.Monad      (mapM_)
import qualified Data.ByteString    as B
import qualified Data.List          as L
import qualified Data.Map           as M
import qualified Data.Set           as S
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Prelude            (IO, Int, Ord, map, take, ($), (+), (.))
import           Text.Printf        (printf)

-- Thanks Yorgey
transMap :: (Ord k, Ord a) => M.Map k a -> M.Map a (S.Set k)
transMap = M.fromListWith S.union . map (second S.singleton . swap) . M.toList
  where swap (one, two) = (two, one)


countWords :: [T.Text] -> M.Map T.Text Int
countWords wordList =
  L.foldl' f M.empty wordList
  where f wordMap word =
          updateWordMap wordMap word
        updateWordMap wordMap word =
          M.insertWith (+) word 1 wordMap

printWordSummary :: (Int, S.Set T.Text) -> IO ()
printWordSummary (count, word) = printf "%7d %s\n" count (S.elemAt 0 word)

main :: IO ()
main = do
  input <- B.getContents
  let text = TE.decodeUtf8 input
      text :: T.Text
      wordList = T.words text
      wordList :: [T.Text]
      wordCounts = countWords wordList
      wordCounts :: M.Map T.Text Int
      topWords = take 10 $ M.toDescList $ transMap wordCounts
      topWords :: [(Int, S.Set T.Text)]
  mapM_ printWordSummary topWords
