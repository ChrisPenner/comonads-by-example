{-# LANGUAGE FlexibleContexts #-}
module Comonads.Store.Newton where

import Control.Comonad
import Control.Comonad.Store
import Control.Arrow
import Comonads.Transformers.Iter
import Data.Function
import Control.Comonad.Env

-- https://en.wikipedia.org/wiki/Newton%27s_method#Square_root_of_a_number

-- Solution for the square root of 612
f :: Double -> Double
f x = (x ^ (2 :: Integer)) - 612

-- Derivative of f
f' :: Double -> Double
f' x = 2 * x

-- Store which extracts (f x, f' x)
lineFuncStore :: Store Double (Double, Double)
lineFuncStore = store (f &&& f') 11

-- Take one step towards the solution to the equation
newtonStep :: ComonadStore Double w => w (Double, Double) -> w (Double, Double)
newtonStep = do
    (fx, dx) <- extract
    seeks (subtract (fx / dx))

fixPointWithinDelta :: (ComonadStore Double w) => Double -> (w (Double, Double) -> w (Double, Double)) -> w (Double, Double) -> w (Double, Double)
fixPointWithinDelta d = iterateUntil (withinDelta `on` pos)
  where
    withinDelta a b = abs (a - b) <= d

solveNewton :: ComonadStore Double w => w (Double, Double) -> w (Double, Double)
solveNewton = fixPointWithinDelta 0.0000001 newtonStep

solveNewtonCount :: EnvT Int (Store Double) (Double, Double) -> EnvT Int (Store Double) (Double, Double)
solveNewtonCount = fixPointWithinDelta 0.0000001 (local (+1) . newtonStep)

-- Given a function alongside its derivative, solve for a root
newtonFix :: Double -> Store Double (Double, Double) -> Double
newtonFix delta w = wfix (extend go w)
  where
    go :: Store Double (Double, Double) -> Store Double Double -> Double
    go s solved = s & do
        (fx, dx) <- extract
        (nextY, _) <- peeks (subtract (fx / dx))
        if abs (nextY - fx) <= abs delta
            then pos
            else return $ peeks (subtract $ fx / dx) solved

