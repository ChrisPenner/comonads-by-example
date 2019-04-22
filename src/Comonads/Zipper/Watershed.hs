module Comonads.Zipper.Watershed where

import Comonads.Zipper
import Control.Comonad

problem :: Zipper Int
problem = fromList [3, 0, 0, 2, 0, 4]

solveSingle :: Zipper Int -> Int
solveSingle (Zipper ls c rs) =
  max 0 $ min (maximum (0 : ls)) (maximum (0 : rs)) - c

solution :: Zipper Int -> Int
solution = sum . extend solveSingle
