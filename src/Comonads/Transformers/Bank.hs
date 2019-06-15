module Comonads.Transformers.Bank where

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Traced
import Control.Comonad.Store

calcSavings :: Store Int Double
calcSavings = store (\mnth -> fromIntegral (mnth * 4000)) 0

v :: EnvT Double (Store Int) Double
v = EnvT 1.1 calcSavings

interest :: (ComonadEnv Double w, ComonadStore Int w) => w Double -> Double
interest = do
    current <- extract
    rate <- ask
    return $ current * rate

