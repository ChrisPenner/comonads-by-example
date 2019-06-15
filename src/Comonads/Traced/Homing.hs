module Comonads.Traced.Homing where

import Comonads.Traced
import Data.Monoid

data Distance = WayTooLow | TooLow | JustRight | TooHigh | WayTooHigh
    deriving Show

game :: Int -> Traced (Sum Int) Distance
game n = traced toDistance
  where
    toDistance (Sum guess)
      | guess > n + 5 = WayTooHigh
      | guess < n - 5 = WayTooLow
      | guess > n = TooHigh
      | guess < n = TooLow
      | otherwise = JustRight

homeIn :: Distance -> Sum Int
homeIn WayTooLow = Sum 3
homeIn TooLow = Sum 1
homeIn JustRight = Sum 0
homeIn TooHigh = Sum (-1)
homeIn WayTooHigh = Sum (-3)


-- homeIn' :: (Distance, Sum Int) -> Sum Int
-- homeIn' (JustRight, x) = x
-- homeIn' (d, _) = homeIn d

-- -- | Use wfix to 'home in' indefinitely
-- homed :: Traced (Sum Int) (Distance, Sum Int)
-- homed = extend wfix (go <$> listen (game 10))
--   where
--     go :: (Distance, Sum Int) -> Traced (Sum Int) (Distance, Sum Int) -> (Distance, Sum Int)
--     go a@(JustRight, _) _ = a
--     go (d, x) w = Debug.trace (show x) $ trace (homeIn d) w

-- λ> trace 12 $ listen t =>> traces (homeIn . fst) =>> traces (homeIn . fst) =>> traces (homeIn . fst)
-- (EQ,Sum {getSum = 10})
