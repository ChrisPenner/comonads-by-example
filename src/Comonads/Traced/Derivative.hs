module Comonads.Traced.Derivative where

import Comonads.Traced
import Data.Monoid
import Control.Comonad

-- Solution for the square root of 612
solveRoot16 :: Double -> Double
solveRoot16 x = (x ^ (2 :: Integer)) - 612

estimateDerivative :: Traced (Sum Double) Double -> Traced (Sum Double) Double
estimateDerivative tracedFunction = extend estimateDerivativeAtPosition tracedFunction
  where
    estimateDerivativeAtPosition :: Traced (Sum Double) Double -> Double
    estimateDerivativeAtPosition w =
        let leftY = trace (Sum (-1)) w
            rightY = trace (Sum 1) w
         in (rightY - leftY) / 2

derivative :: (Double -> Double) -> Double -> Double
derivative f = runTraced tracedDerivative . Sum
  where
    tracedF = Traced (f . getSum)
    tracedDerivative = estimateDerivative tracedF

derivativeRoot16 :: Double -> Double
derivativeRoot16 = derivative solveRoot16
