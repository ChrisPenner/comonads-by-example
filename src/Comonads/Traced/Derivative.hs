module Comonads.Traced.Derivative where

import Comonads.Traced
import Control.Comonad.Env
import Data.Monoid

-- Solution for the square root of 612
solveRoot16 :: Double -> Double
solveRoot16 x = (x ^ (2 :: Integer)) - 16

solveRoot16T :: Traced (Sum Double) Double
solveRoot16T  = traced (solveRoot16 . getSum)

estimateDerivativeAtPosition :: Traced (Sum Double) Double
                             -> Double
estimateDerivativeAtPosition w =
    let leftY = trace (Sum (-1)) w
        rightY = trace (Sum 1) w
        in (rightY - leftY) / 2

-- | The same as estimateDerivativeAtPosition but in Reader form
estimateDerivativeAtPositionReader :: Traced (Sum Double) Double
                             -> Double
estimateDerivativeAtPositionReader = do
    leftY <- trace (Sum (-1))
    rightY <- trace (Sum 1)
    return $ (rightY - leftY) / 2

estimateDerivative :: Traced (Sum Double) Double
                   -> Traced (Sum Double) Double
estimateDerivative = extend estimateDerivativeAtPosition

withDerivative :: Traced (Sum Double) (Double, Double)
withDerivative = liftW2 (,) solveRoot16T (estimateDerivative solveRoot16T)

derivativeOf :: (Double -> Double) -> (Double -> Double)
derivativeOf f = runTraced tracedDerivative . Sum
  where
    tracedF = traced (f . getSum)
    tracedDerivative = estimateDerivative tracedF
