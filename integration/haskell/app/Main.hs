{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Lib     (calcInt, numIntAndVol, solve, squareFormula)
import           Prelude (IO, pi, print, sum, zip, ($), (*), (-), (.), (<$>),
                          (<*>))

main :: IO ()
main = do
  let ans     = solve 1 4 [1..5] [6..10]
      ansN    = numIntAndVol 1 4 [1..5] [6..10]
      ans'    = solve 2 20 [1,2] [0,1]
      ans'N   = numIntAndVol 2 20 [1,2] [0,1]
      squared = squareFormula $ zip [1,2] [0,1]
      sqUpper = (pi*) . sum $ calcInt <$> squared <*> [20.0]
      sqLower = (pi*) . sum $ calcInt <$> squared <*> [2.0]
      vol     = sqUpper - sqLower
  print "answer one: "
  print ans
  print "Answer one, Numeric:"
  print ansN
  print "answer two: "
  print ans'
  print "Answer two, Numeric:"
  print ans'N
  print "squared: "
  print squared
  print "square formula: "
  print sqUpper
  print sqLower
  print vol
  -- getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
