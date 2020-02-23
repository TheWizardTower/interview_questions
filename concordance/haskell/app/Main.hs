{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Control.Arrow       (second)
import           Control.Monad       (mapM_)
import qualified Data.ByteString     as B
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Semigroup      ((<>))
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import           Options.Applicative as A
import           Prelude             (IO, Int, Ord, map, take, ($), (+), (.))
import           System.IO           (IOMode (..), withFile)
import           Text.Printf         (printf)

data CommandArguments = CommandArguments
  { filename :: T.Text
  , numWords :: Int
  }

cliParser :: Parser CommandArguments
cliParser = CommandArguments
  <$> A.strOption
   ( long "filename"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Filename to read in as input. If left blank, will read from stdin."
  <> value (T.pack "")
  <> showDefault
   ) <*> A.option A.auto
   ( long "number-of-words"
  <> short 'n'
  <> metavar "NUMBER"
  <> help "Number of most-used words to print. Defaults to 10."
  <> value 10
  <> showDefault
   )

readFromFileOrStdin :: T.Text -> IO B.ByteString
readFromFileOrStdin fname =
  if T.null fname then B.getContents
  else withFile (T.unpack fname) ReadMode B.hGetContents


-- Thanks Yorgey
transMap :: (Ord k, Ord a) => M.Map k a -> M.Map a (S.Set k)
transMap = M.fromListWith S.union . map (second S.singleton . swap) . M.toList
  where swap (one, two) = (two, one)


countWords :: [T.Text] -> M.Map T.Text Int
countWords = L.foldl' updateWordMap M.empty
  where  updateWordMap wordMap word =
          M.insertWith (+) word 1 wordMap

printWordSummary :: (Int, S.Set T.Text) -> IO ()
printWordSummary (count, word) = printf "%7d %s\n" count (S.elemAt 0 word)

main :: IO ()
main = do
  let opts = info (cliParser <**> helper)
        ( fullDesc
        <> progDesc "Build a concordance from an input file, and show the N most frequent words."
        <> header "Concordance-Haskell -- a concordance builder for input files built in Haskell."
        )
  args <- execParser opts
  input <- readFromFileOrStdin $ filename args
  let text = TE.decodeUtf8 input
      text :: T.Text
      wordList = T.words text
      wordList :: [T.Text]
      wordCounts = countWords wordList
      wordCounts :: M.Map T.Text Int
      topWords = take (numWords args) $ M.toDescList $ transMap wordCounts
      topWords :: [(Int, S.Set T.Text)]
  mapM_ printWordSummary topWords
