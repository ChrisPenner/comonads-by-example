{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
module Comonads.Zipper where

import Control.Comonad
import Data.List
import Data.Maybe

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

moveRight' :: Zipper a -> Zipper a
moveRight' z = fromMaybe z $ moveRight z

moveLeft' :: Zipper a -> Zipper a
moveLeft' z = fromMaybe z $ moveLeft z

moveRight :: Zipper a -> Maybe (Zipper a)
moveRight (Zipper ls c (r : rs)) = Just $ Zipper (c : ls) r rs
moveRight _                      = Nothing

instance Comonad Zipper where
  extract :: Zipper a -> a
  extract = focus
  duplicate :: Zipper a -> Zipper (Zipper a)
  duplicate z = Zipper ls' z rs'
    where
      ls' = unfoldr (fmap pair . moveLeft) z
      rs' = unfoldr (fmap pair . moveRight) z
      pair :: a -> (a, a)
      pair a = (a, a)

