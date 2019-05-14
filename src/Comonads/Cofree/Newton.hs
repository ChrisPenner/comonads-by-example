module Comonads.Cofree.Newton where

import Control.Comonad.Cofree
import Data.Functor.Identity

f :: Double -> Double
f x = (x ^ (2 :: Integer)) - 612

-- Derivative of f
f' :: Double -> Double
f' x = 2 * x

steps :: Double -> Cofree Maybe Double
steps delta = coiter coalg 10
  where
    coalg :: Double -> Maybe Double
    coalg x = case x - (f x / f' x) of
        next
            | abs (next - x) < delta -> Nothing
            | otherwise -> Just next

steps' ::  Cofree Identity Double
steps' = coiter coalg 10
  where
    coalg :: Double -> Identity Double
    coalg x = pure $ x - (f x / f' x)
