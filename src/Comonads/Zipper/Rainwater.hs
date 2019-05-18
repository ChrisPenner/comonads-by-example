module Comonads.Zipper.Rainwater where

import Comonads.Zipper
import Control.Comonad

problem :: Zipper Int
problem = fromList [2, 0, 4, 2, 3, 2, 1, 2]
-- > Zipper [] 2 [0, 4, 2, 3, 2, 1, 2]

max0 :: [Int] -> Int
max0 [] = 0
max0 xs = maximum xs

waterAtPosition :: Zipper Int -> Int
waterAtPosition (Zipper toLeft current toRight)
  = max 0 (containingWallHeight - current)
  where
    containingWallHeight = min (max0 toLeft) (max0 toRight)

solution :: Zipper Int -> Int
solution z = sum (extend waterAtPosition z)
