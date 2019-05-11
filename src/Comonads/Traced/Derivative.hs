module Comonads.Traced.Derivative where

import Comonads.Traced
import Control.Comonad.Env
import Data.Monoid
import Control.Arrow ((&&&))

-- Solution for the square root of 612
solveRoot16 :: Double -> Double
solveRoot16 x = (x ^^ (2 :: Integer)) - 612

solveRoot16T :: Traced (Sum Double) Double
solveRoot16T  = traced (solveRoot16 . getSum)

solveRoot16T' :: Traced (Sum Double) (Double, Double)
solveRoot16T'  =  alongsideW estimateDerivative solveRoot16T

alongsideW :: Comonad w => (w a -> w b) -> w a -> w (a, b)
alongsideW f = extend (extract &&& extract . f)

estimateDerivative :: Traced (Sum Double) Double -> Traced (Sum Double) Double
estimateDerivative tracedFunction = extend estimateDerivativeAtPosition tracedFunction
  where
    estimateDerivativeAtPosition :: Traced (Sum Double) Double -> Double
    estimateDerivativeAtPosition w =
        let leftY = trace (Sum (-1)) w
            rightY = trace (Sum 1) w
         in (rightY - leftY) / 2

derivative :: (Double -> Double) -> (Double -> Double)
derivative f = runTraced tracedDerivative . Sum
  where
    tracedF = traced (f . getSum)
    tracedDerivative = estimateDerivative tracedF

derivativeRoot16 :: Double -> Double
derivativeRoot16 = derivative solveRoot16

fAndf' :: Traced (Sum Double) (Double, Double)
fAndf' = alongsideW estimateDerivative solveRoot16T
