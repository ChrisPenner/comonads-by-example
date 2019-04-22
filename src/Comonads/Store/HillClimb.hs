{-# LANGUAGE MultiWayIf #-}
module Comonads.Store.HillClimb where

import Comonads.Transformers.Iter
import Comonads.Store
import Control.Comonad
import Data.Function

func :: Store Double Double
func = Store f 20
  where
    f x = (negate (x ** 1.2)) + 20

sinFunc :: Store Double Double
sinFunc = Store f 20
  where
    f x = sin x * 100

move :: Store Double Double -> Store Double Double
move s = let left = seeks (subtract 1) s
             right = seeks (+1) s
          in if | extract s > extract left && extract s > extract right -> s
                | extract left > extract right -> left
                | otherwise -> right

findBest :: Store Double Double -> Store Double Double
findBest = iterateUntil (inDelta `on` extract)  move
  where
    inDelta a b = abs (a - b) < 0.001

maxFindableFromPos :: Store Double Double
maxFindableFromPos = extend (pos . findBest) sinFunc
