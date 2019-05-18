module Comonads.Env.Clamp where

import Comonads.Env
import Control.Comonad
import Data.Bifunctor

type Range = (Int, Int)
clamp :: Env Range Int -> Int
clamp w = let (lowest, highest) = ask w
           in max lowest . min highest . extract $ w

moveBy :: Int -> Env Range Int -> Int
moveBy n = clamp . fmap (+n)

moveTo :: Int -> Env Range Int -> Int
moveTo n = clamp . fmap (const n)

adjustUpperBy :: Int -> Env Range Int -> Env Range Int
adjustUpperBy n = local (second (+n))

adjustLowerBy :: Int -> Env Range Int -> Env Range Int
adjustLowerBy n = local (first (+n))

x :: Env (Int, Int) Int
x = Env (0, 5) 3
