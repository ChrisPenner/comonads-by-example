module Comonads.Traced.Derivative where

import Comonads.Traced
import Control.Comonad.Env
import Data.Monoid

-- Solution for the square root of 612
func :: Sum Double -> Double
func (Sum x) = (x ^ (2 :: Integer)) - 16

f :: Traced (Sum Double) Double
f  = traced func

estimateDerivative :: Traced (Sum Double) Double
                             -> Double
estimateDerivative w =
    let leftY = trace (Sum (-1)) w
        rightY = trace (Sum 1) w
        in (rightY - leftY) / 2

-- | The same as estimateDerivativeAtPosition but in Reader form
estimateDerivativeReader :: Traced (Sum Double) Double
                             -> Double
estimateDerivativeReader = do
    leftY <- trace (Sum (-1))
    rightY <- trace (Sum 1)
    return $ (rightY - leftY) / 2

estimateDerivativeOf :: Traced (Sum Double) Double
                   -> Traced (Sum Double) Double
estimateDerivativeOf = extend estimateDerivative

withDerivative :: Traced (Sum Double) (Double, Double)
withDerivative = liftW2 (,) f (estimateDerivativeOf f)

derivativeOf :: (Double -> Double) -> (Double -> Double)
derivativeOf g = runTraced tracedDerivative . Sum
  where
    tracedF = traced (g . getSum)
    tracedDerivative = estimateDerivativeOf tracedF
