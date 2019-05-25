{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}

module Comonads.Store where

import Control.Comonad

data Store s a = Store (s -> a) s
    deriving (Functor, ComonadApply)

store :: (s -> a) -> s -> Store s a
store = Store

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) =
      Store (\s' -> Store f s') s
  extend g st = g <$> duplicate st

instance Monoid s => Applicative (Store s) where
  pure a = Store (const a) mempty
  Store a i <*> Store b j = Store (a <*> b) (i <> j)

pos :: Store s a -> s
pos (Store _ s) = s

peek :: s -> Store s a -> a
peek s (Store f _) = f s

peeks :: (s -> s) -> Store s a -> a
peeks g (Store f s) = f (g s)

seek :: s -> Store s a -> Store s a
seek s (Store f _) = Store f s

seeks :: (s -> s) -> Store s a -> Store s a
seeks g (Store f s) = Store f (g s)

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment search (Store f s) = f <$> search s

