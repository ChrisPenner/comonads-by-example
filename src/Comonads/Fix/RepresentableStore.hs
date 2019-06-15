module Comonads.Fix.RepresentableStore where

import Data.Stream.Infinite
import Control.Comonad
import Control.Comonad.Representable.Store
import Data.Function (fix)
import Debug.Trace

-- Here we observe that using a Representable structure as our Comonad Backing gives us
-- 'caching'; however only on a single value basis
factorialStreamW :: Store Stream Int
factorialStreamW = extend wfix $ store (flip go) 0
  where
    go _ 0 = 1
    go w n = trace ("executing: " <> show n) n * peek (n - 1) w


-- If we manually use 'fix' we get even BETTER sharing; it'll only calculate each value
-- exactly once
factorialStreamF :: Store Stream Int
factorialStreamF = fix $ \result -> store (go result) 0
  where
    go _ 0 = 1
    go w n = trace ("executing: " <> show n) n * peek (n - 1) w



-- λ> peek 2 factorialStreamW
-- executing: 2
-- executing: 1
-- 2
-- λ> peek 3 factorialStreamW
-- executing: 3
-- executing: 2
-- executing: 1
-- 6
-- λ> peek 3 factorialStreamW
-- 6

-- λ> peek 2 factorialStreamF
-- executing: 2
-- executing: 1
-- 2
-- λ> peek 3 factorialStreamF
-- executing: 3
-- 6
-- λ> peek 3 factorialStreamF
-- 6
