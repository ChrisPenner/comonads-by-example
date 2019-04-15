{-# LANGUAGE DeriveFunctor #-}
module Function.Derivative where

import Control.Comonad
import Control.Comonad.Store

-- Solution for the square root of 612
f :: Double -> Double
f x = (x ^ (2 :: Integer)) - 612

data Pair a = Pair a a
    deriving (Show, Eq, Functor)

fStore :: Store Double Double
fStore = store f 10

fStore' :: Store Double Double
fStore' = extend go fStore
    where
        go :: Store Double Double -> Double
        go = slope . experiment neighbours
        neighbours :: Double -> Pair Double
        neighbours x = Pair (x-1) (x+1)
        slope :: Pair Double -> Double
        slope (Pair l r) = (r - l) / 2

f' :: Double -> Double
f' = fst $ runStore fStore'

-- Derivative of f
-- f' :: Double -> Double
-- f' x = 2 * x
