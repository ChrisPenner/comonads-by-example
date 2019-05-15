{-# LANGUAGE DeriveFunctor #-}
module Comonads.Env where

import Control.Comonad

data Env e a = Env e a
    deriving (Eq, Show, Functor)

instance Comonad (Env e) where
  extract (Env _ a) = a
  duplicate w@(Env e _) = Env e w
  extend f w@(Env e _) = Env e (f w)

env :: e -> a -> Env e a
env = Env

ask :: Env e a -> e
ask (Env e _) = e

asks :: (e -> e') -> Env e a -> e'
asks f (Env e _) = f e

local :: (e -> e') -> Env e a -> Env e' a
local f (Env e a) = Env (f e) a
