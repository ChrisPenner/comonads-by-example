module Comonads.Env.Clamp where

import Comonads.Env
import Control.Comonad
import Data.Bifunctor

type Range = (Int, Int)
clamp :: Env Range Int -> Int
clamp w = let (lowest, highest) = ask w
           in max lowest . min highest . extract $ w

moveBy :: Int -> Env Range Int -> Int
moveBy n = extract . fmap (+n)

moveTo :: Int -> Env Range Int -> Int
moveTo n = clamp . fmap (+n)

adjustUpper :: Int -> Env Range Int -> Env Range Int
adjustUpper n = local (second (+n))

adjustLower :: Int -> Env Range Int -> Env Range Int
adjustLower n = local (first (+n))

x :: Env (Int, Int) Int
x = Env (0, 5) 3
