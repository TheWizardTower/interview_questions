{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( calcInt
    , numIntAndVol
    , solve
    , squareFormula
    ) where

import           Data.Foldable (foldl')
import           Data.Map      (Map, foldlWithKey', fromList, insertWith)
import           Prelude       (Double, Int, fromIntegral, pi, sum, zip, ($),
                                (*), (**), (+), (-), (.), (/), (<$>), (<*>))

calcFunc :: (Double, Double) -> Double -> Double
calcFunc (coeff,expn) x = coeff * (x ** expn)

calcInt :: (Double, Double) -> Double -> Double
calcInt (coeff, expn) = calcFunc (coeff/(expn+1), expn+1)

multPair :: (Double, Double) -> (Double, Double) -> (Double, Double)
multPair (c1,e1) (c2,e2) = (c1 * c2, e1 + e2)

unpackMap :: Map k v -> [(v,k)]
unpackMap = foldlWithKey' (\lst expn coeff -> (coeff, expn) : lst) []

multPairIntoMap :: (Double, Double) -> Map Double Double -> (Double, Double) -> Map Double Double
multPairIntoMap (mc, me) argMap (c,e) =
  let (c', e') = multPair (mc, me) (c,e)
      newMap = insertWith (+) e' c' argMap in
  newMap

multFormulaHelper :: [(Double, Double)] -> [(Double, Double)] -> Map Double Double -> [(Double, Double)]
multFormulaHelper [] _ hMap = unpackMap hMap
multFormulaHelper ((coeff,expn):pairs) formPairs hMap =
  let map' = foldl' (multPairIntoMap (coeff,expn)) hMap formPairs in
    multFormulaHelper pairs formPairs map'

squareFormula :: [(Double, Double)] -> [(Double, Double)]
squareFormula formula = multFormulaHelper formula formula (fromList [])

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b =  solveP (fromIntegral l) (fromIntegral r) (fromIntegral <$> a) (fromIntegral <$> b)

solveP :: Double -> Double -> [Double] -> [Double] -> [Double]
solveP lower upper coeffs expns =
  let formula = zip coeffs expns
      sqFormula = squareFormula formula
      integralPairs = calcInt <$> formula
      sqIntPairs    = calcInt <$> sqFormula
      upperA  = sum $ integralPairs <*> [upper]
      lowerA  = sum $ integralPairs <*> [lower]
      upperV  = (pi*) . sum $ sqIntPairs <*> [upper]
      lowerV  = (pi*) . sum $ sqIntPairs <*> [lower] in
  [upperA - lowerA, (upperV - lowerV)]

-- This solution is borrowed/translated from
-- https://alexatnet.com/hr-f-area-under-curves-and-volume-of-revolving-a-curve/
foldFunc :: (Double, Double) -> Double -> (Double, Double)
foldFunc (area, volume) y = (area + 0.001 * y, volume + 0.001 * y * y * pi)

numIntAndVol :: Double -> Double -> [Double] -> [Double] -> (Double, Double)
numIntAndVol lower upper coeffs exps =
  let fun     = zip coeffs exps
      inputs  = [lower,lower+0.001..upper]
      formula = calcFunc <$> fun
      values  = (\x -> sum $ formula <*> [x]) <$> inputs
      resultTuple = foldl' (foldFunc) (0.0,0.0) values in
  resultTuple
