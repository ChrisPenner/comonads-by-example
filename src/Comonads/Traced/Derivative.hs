module Comonads.Traced.Derivative where

-- import Comonads.Traced
import Control.Comonad.Traced (traces)
import Control.Comonad.Trans.Traced
import Control.Comonad.Env
import Data.Monoid
import Control.Comonad
import Control.Arrow ((&&&))
import Comonads.Transformers.Iter
import Control.Comonad.Hoist.Class

-- Solution for the square root of 612
solveRoot16 :: Double -> Double
solveRoot16 x = (x ^^ (2 :: Integer)) - 612

solveRoot16T :: Traced (Sum Double) Double
solveRoot16T  = traced (solveRoot16 . getSum)

solveRoot16T' :: Traced (Sum Double) (Double, Double)
solveRoot16T'  =  alongsideW estimateDerivative solveRoot16T


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
    tracedF = traced (f . getSum)
    tracedDerivative = estimateDerivative tracedF

derivativeRoot16 :: Double -> Double
derivativeRoot16 = derivative solveRoot16

alongsideW :: Comonad w => (w a -> w b) -> w a -> w (a, b)
alongsideW f = extend (extract &&& extract . f)

thingy :: Traced (Sum Double) (Double, Double)
thingy = alongsideW estimateDerivative solveRoot16T

newtonStep :: Traced (Sum Double) (Double, Double) -> (Double, Double)
newtonStep = traces delta

newtonStep' :: EnvT (Sum Double) (Traced (Sum Double)) (Double, Double)
            -> EnvT (Sum Double) (Traced (Sum Double)) (Double, Double)
newtonStep' w = let next = extend (traces (delta . fst) . listen . lower) $ w
                 in fmap fst $ local (mappend (snd $ extract next)) next

delta :: (Double, Double) -> Sum Double
delta (fx, dx) = Sum . negate $ fx / dx

fixPointWithinDelta :: (Comonad w) => Double -> (w (Double, Double) -> (Double, Double)) -> w (Double, Double) -> w (Double, Double)
fixPointWithinDelta d = extendUntil2 withinDelta
  where
    withinDelta (a, _) (b, _) = abs (a - b) <= d

solveNewton :: (Double -> Double) -> Double
solveNewton f =
    fst . extract
    $ fixPointWithinDelta 1
                          newtonStep
                          (alongsideW estimateDerivative (traced (f . getSum)))
