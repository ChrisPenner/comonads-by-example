module Comonads.Fix.CFix where

import Control.Comonad
import Control.Comonad.Store
import Data.List.NonEmpty
import Control.Comonad.Traced

-- factCFix :: Store Int Int
-- factCFix = cfix go
--   where
--     go :: Store Int Int -> Int
--     go w = pos w
    -- go w | pos w == 0 = 1
    --      | otherwise = pos w * peeks (subtract 1) w


fby :: Store Int a -> Store Int a -> a
fby a b = if pos a == 0 && pos b == 0 then peek 0 a else peeks (subtract 1) b

nat :: Store Int a -> Int
nat w = extract (w =>> (+1) . nat =>> fby (store (const 0) 0))

nat' :: Store Int Int
nat' = cfix $ \w -> extract (seek 0 w =>> (+1) . extract =>> fby (store (const 0) 0))
