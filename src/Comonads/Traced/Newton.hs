module Comonads.Traced.Newton where

import Comonads.Traced.Derivative
import Comonads.Traced
import Control.Comonad.Env
import Data.Monoid
import Comonads.Transformers.Iter

newtonStep :: Traced (Sum Double) (Double, Double) -> (Double, Double)
newtonStep = traces delta

delta :: (Double, Double) -> Sum Double
delta (fx, dx) = Sum . negate $ fx / dx

fixPointWithinDelta :: (Comonad w) => Double -> (w (Double, Double) -> (Double, Double)) -> w (Double, Double) -> w (Double, Double)
fixPointWithinDelta d = extendUntil2 withinDelta
  where
    withinDelta (a, _) (b, _) = abs (a - b) <= d

pairDerivative :: (Double -> Double) -> Traced (Sum Double) (Double, Double)
pairDerivative f = extend (trace (Sum 1)) $ alongsideW estimateDerivative (traced (f . getSum))

solveNewton :: (Double -> Double) -> Double
solveNewton f =
    fst . extract
    $ fixPointWithinDelta 10 newtonStep (pairDerivative f)
