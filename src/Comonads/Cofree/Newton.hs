module Comonads.Cofree.Newton where

import Control.Comonad.Cofree
import Data.Functor.Identity

f :: Double -> Double
f x = (x ^ (2 :: Integer)) - 612

-- Derivative of f
f' :: Double -> Double
f' x = 2 * x

-- Build list of Newton's iterations until the difference between iterations is less than delta
steps :: Double -> Double -> Cofree Maybe Double
steps start delta = coiter coalg start
  where
    coalg :: Double -> Maybe Double
    coalg x = case x - (f x / f' x) of
        next
            | abs (next - x) < delta -> Nothing
            | otherwise -> Just next

-- Calculate infinite newton's method iterations
steps' ::  Double -> Cofree Identity Double
steps' start = coiter coalg start
  where
    coalg :: Double -> Identity Double
    coalg x = pure $ x - (f x / f' x)
