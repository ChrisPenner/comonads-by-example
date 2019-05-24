module Comonads.Env.Default where

import Comonads.Env

reset :: Env e a -> e
reset = ask
