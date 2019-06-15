module Comonads.Traced.Zipper where

import Comonads.Zipper
import Comonads.Traced
import Control.Comonad
import Data.Monoid

data Dir = L | R
    deriving (Show, Eq)

zipper :: Zipper Int
zipper = fromList [1,2,3,4,5]

-- Navigate
tZipper :: Zipper a -> Traced (Dual [Dir]) a
tZipper z = traced (extract . follow z . getDual)
  where
    follow :: Zipper a -> [Dir] -> Zipper a
    follow z' [] = z'
    follow z' (L : rest) = follow (moveLeft' z') rest
    follow z' (R : rest) = follow (moveRight' z') rest



