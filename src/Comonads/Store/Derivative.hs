{-# LANGUAGE DeriveFunctor #-}
module Comonads.Store.Derivative where

import Control.Comonad
import Comonads.Store

-- Solution for the square root of 612
f :: Double -> Double
f x = (x ^ (2 :: Integer)) - 612

data Pair a = Pair a a
    deriving (Show, Eq, Functor)

fStore :: Store Double Double
fStore = Store f 10

fStore' :: Store Double Double
fStore' = extend estimateDerivative fStore
  where
    estimateDerivative :: Store Double Double -> Double
    estimateDerivative = slope . experiment neighbours

    neighbours :: Double -> Pair Double
    neighbours x = Pair (x - 1) (x + 1)

    slope :: Pair Double -> Double
    slope (Pair l r) = (r - l) / 2

f' :: Double -> Double
f' = flip peek fStore'
