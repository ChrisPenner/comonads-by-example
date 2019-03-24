module Store.Newton where

import Control.Comonad
import Control.Comonad.Store
import Control.Arrow

-- https://en.wikipedia.org/wiki/Newton%27s_method#Square_root_of_a_number


-- step :: Double -> Double
-- step x = x - (f x / f' x)

-- lineStore :: Store Double Double
-- lineStore = store f 10

-- stepStore :: Store Double Double -> Store Double Double
-- stepStore w = seeks (subtract (extract w / f' (pos w))) w

-- stepStoreR :: Store Double Double -> Store Double Double
-- stepStoreR = do
--   currentVal <- extract
--   derivative <- f' . pos
--   let offset = currentVal / derivative
--   seeks (subtract offset)

-- Solution for the square root of 612
f :: Double -> Double
f x = (x ^ (2 :: Integer)) - 612

-- Derivative of f
f' :: Double -> Double
f' x = 2 * x

-- Store which extracts (f x, f' x)
lineFuncStore :: Store Double (Double, Double)
lineFuncStore = store (f &&& f') 10

-- Take one step towards the solution to the equation
newtonStep :: Store Double (Double, Double) -> Store Double (Double, Double)
newtonStep = do
  (x, dx) <- extract
  seeks (subtract (x / dx))
