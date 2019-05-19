module Comonads.Traced.Zipper where

import Comonads.Zipper
import Comonads.Traced
import Control.Comonad

data Dir = L | R
    deriving (Show, Eq)

z :: Zipper Int
z = fromList [1,2,3,4,5]

tz :: Zipper a -> Traced [Dir] a
tz zipper = traced (extract . follow zipper)
  where
    follow :: Zipper a -> [Dir] -> Zipper a
    follow z' [] = z'
    follow z' (L : rest) = follow (moveLeft' z') rest
    follow z' (R : rest) = follow (moveRight' z') rest



