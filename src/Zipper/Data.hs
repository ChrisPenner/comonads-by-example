{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Zipper.Data where

import Control.Comonad
import Data.List

data Zipper a =
  Zipper
    { left :: [a]
    , focus :: a
    , right :: [a]
    } deriving (Show, Eq, Functor, Foldable)

fromList :: [a] -> Zipper a
fromList []       = error "fromList on empty list for Zipper"
fromList (x : xs) = Zipper [] x xs

moveLeft :: Zipper a -> Maybe (Zipper a)
moveLeft (Zipper (l : ls) c rs) = Just $ Zipper ls l (c : rs)
moveLeft _                      = Nothing

moveRight :: Zipper a -> Maybe (Zipper a)
moveRight (Zipper ls c (r : rs)) = Just $ Zipper (c : ls) r rs
moveRight _                      = Nothing

instance Comonad Zipper where
  extract = focus
  duplicate z = Zipper ls' z rs'
    where
      ls' = unfoldr (fmap pair . moveLeft) z
      rs' = unfoldr (fmap pair . moveRight) z
      pair :: a -> (a, a)
      pair a = (a, a)

problem :: Zipper Int
problem = fromList [3, 0, 0, 2, 0, 4]

solveSingle :: Zipper Int -> Int
solveSingle (Zipper ls c rs) =
  max 0 $ min (maximum (0 : ls)) (maximum (0 : rs)) - c

solution :: Zipper Int -> Int
solution = sum . extend solveSingle
