module Comonads.Fix.WFix where

import Control.Comonad
import Comonads.Store
import Comonads.Traced
import Data.Monoid
import qualified Data.List.NonEmpty as NE

factorialStore :: Store Int Int
factorialStore = extend wfix (store go 0)
    where
      go :: Int -> (Store Int Int -> Int)
      go 0 _ = 1
      go n w = n * peeks (subtract 1) w

factorialTraced :: Traced (Sum Int) Int
factorialTraced = extend wfix (traced go)
  where
    go :: Sum Int -> Traced (Sum Int) Int -> Int
    go (Sum 0) _ = 1
    go (Sum n) t = n * trace (-1) t

factorialStore2 :: Store Int Int
factorialStore2 = extend wfix (Store (\_s -> go) 0)
    where
      go :: Store Int Int -> Int
      go s | pos s == 0 = 1
      go w = pos w * peeks (subtract 1) w

factorialStore3 :: Store Int Int
factorialStore3 = extend wfix (go <$> idStore)
    where
      idStore :: Store Int Int
      idStore = store id 0
      go :: Int -> Store Int Int -> Int
      go 0 _ = 1
      go n w = n * peek (n - 1) w

-- | Sum of current and following numbers at each position
-- λ> sums (NE.fromList [1,2,3,4])
-- 10 :| [9,7,4]
sums :: NE.NonEmpty Int -> NE.NonEmpty Int
sums w = extend wfix (go <$> w)
  where
    go :: Int -> NE.NonEmpty Int -> Int
    go n (_ NE.:| []) = n
    go n (_ NE.:| (x:_)) = n + x
