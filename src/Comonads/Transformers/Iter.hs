{-# LANGUAGE ScopedTypeVariables #-}
module Comonads.Transformers.Iter where

import Control.Comonad.Env

extendUntilFixedPoint :: forall w a. (Comonad w, Eq a) => (w a -> a) -> w a -> w a
extendUntilFixedPoint f w = extendUntil2 (==) f w


extendUntil2 :: forall w a. Comonad w => (a -> a -> Bool) -> (w a -> a) -> w a -> w a
extendUntil2 check f w' = lower . extend go $ EnvT (extract w') w'
  where
    go :: EnvT a w a -> a
    go w =
        if check (ask w) (f (lower w))
            then ask w
            else go $ EnvT (f (lower w)) (extend f (lower w))

extendUntil :: forall w a. Comonad w => (a -> Bool) -> (w a -> a) -> w a -> w a
extendUntil check = extendUntil2 (const check)

iterateUntil :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterateUntil check f a =
    if check a (f a)
        then f a
        else iterateUntil check f (f a)
