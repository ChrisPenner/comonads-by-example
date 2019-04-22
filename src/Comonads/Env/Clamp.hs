module Comonads.Env.Clamp where

import Comonads.Env
import Control.Comonad
import Data.Bifunctor

type Range = (Int, Int)
type Location = Env (Int, Int) Int

clamp :: Location -> Int
clamp l = let (lowest, highest) = ask l
           in max lowest . min highest . extract $ l

move :: Int -> Location -> Location
move n = fmap (+n)

adjustUpper :: Int -> Location -> Location
adjustUpper n = local (second (+n))

adjustLower :: Int -> Location -> Location
adjustLower n = local (first (+n))
