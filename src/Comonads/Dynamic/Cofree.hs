{-# LANGUAGE FlexibleContexts #-}
module Comonads.Dynamic.Cofree where

import Data.Semigroup
import Control.Comonad
import Control.Comonad.Store
import Debug.Trace
import Control.Arrow
import qualified Data.List.NonEmpty as NE

dynFix :: Comonad w => (w b -> b) -> w a -> w b
dynFix f w = fmap f (extend (dynFix f) w)

dynFix' :: Comonad w => ((a, w b) -> b) -> w a -> w b
dynFix' f w = fmap f (extend (extract &&& dynFix' f) w)

dynFixShare :: ComonadApply w => ((a, w b) -> b) -> w a -> w b
dynFixShare f w = let x = extend (dynFixShare f) w
                   in liftW2 (curry f) w x

dynKFix :: ComonadApply w => ((a, w b) -> b) -> w a -> w b
dynKFix f w = kfix (fmap (curry f) w)

dynWFix :: Comonad w => ((a, w b) -> b) -> w a -> b
dynWFix f w = wfix (fmap (curry f) w)

dynWFix' :: Comonad w => ((a, w b) -> b) -> w a -> w b
dynWFix' f w = extend wfix (fmap (curry f) w)

-- dynFix'' :: ComonadApply w => (a -> w b -> b) -> w a -> w b
-- dynFix'' f w = let r = liftW2 f w (duplicate r)
--                in r

-- factStore :: Store (Sum Int) Int
-- factStore = dynFix'' go idStore
--     where
--       idStore = store id 0
--       go :: Store (Sum Int) Int -> Int
--       go w | pos w == Sum 0 = 1
--       go w = traceShow (pos w) $ getSum (pos w) * peeks (subtract 1) w

-- factStore' :: Store Int Int
-- factStore' = dynFix'' go idStore
--     where
--       idStore = store id 0
--       go :: (Int, Store Int Int) -> Int
--       go (0, _) = 1
--       go (n, w) = n * peeks (subtract 1) w


factStore' :: Store Int Int
factStore' = dynWFix' go idStore
    where
      idStore = store id 3
      go :: (Int, Store Int Int) -> Int
      go (0, _) = 1
      go (a, w) = a * peek (a - 1) w

sums :: NE.NonEmpty Int -> NE.NonEmpty Int
sums = dynFix' go
  where
    go :: (Int, NE.NonEmpty Int) -> Int
    go (n, (_ NE.:| [])) = trace ("adding: " ++ show n ++ "\n") $ n
    go (n, _ NE.:| (x:_)) = trace ("adding: " ++ show n ++ "\n") $ n + x

sumsShare :: NE.NonEmpty Int -> NE.NonEmpty Int
sumsShare = dynWFix' go
  where
    go :: (Int, NE.NonEmpty Int) -> Int
    go (n, (_ NE.:| [])) = trace ("adding: " ++ show n ++ "\n") $ n
    go (n, _ NE.:| (x:_)) = trace ("adding: " ++ show n ++ "\n") $ n + x
