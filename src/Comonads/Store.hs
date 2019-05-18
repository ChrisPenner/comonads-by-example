{-# LANGUAGE DeriveFunctor #-}

module Comonads.Store where

import Control.Comonad

data Store s a = Store (s -> a) s
    deriving Functor

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) =
      Store (\s' -> Store f s') s
  extend g store = g <$> duplicate store

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

