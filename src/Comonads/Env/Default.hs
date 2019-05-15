module Comonads.Env.Default where

import Comonads.Env
import Control.Comonad

reset :: Env e a -> e
reset = ask
