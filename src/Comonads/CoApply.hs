{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Comonads.CoApply where

import Data.Functor.Rep
import Data.Distributive

import Control.Comonad.Cofree
import Data.Functor.Identity

-- import Control.Comonad
-- import Comonads.Stream
-- import Comonads.Store
-- import Control.Comonad.Traced
-- import Data.Function (fix)

coap :: (Representable f) => (f a -> f b) -> f (a -> b)
coap f = tabulate (\i -> flip index i . f . pureRep)

next :: Cofree Identity Int -> Cofree Identity Int
next (_ :< Identity (b :< xs)) = b :< xs

nexted :: Cofree Identity (Int -> Int)
nexted = coap next

third :: Cofree Identity a -> a
third (_ :< Identity (_ :< Identity (x :< _))) = x

count :: Cofree Identity Int
count = 1 :< Identity (fmap (+1) count)

t :: Cofree Identity (Int -> Int)
t = coap $ mzipWithRep (+) count


-- class Comonad f => CoApply f where
--   copure :: f a -> a
--   coap :: (f a -> f b) -> f (a -> b)
--   coap f = fix (\x -> ((copure . f . (x $>)) :> x))

-- instance CoApply Stream where
--   copure = extract
--   -- coap f = cycleS (extract . f . cycleS)
--   coap f = fix (\x -> ((extract . f . (x $>)) :> x))


-- instance (Monoid m) => CoApply (Traced m) where
--   copure = extract
--   coap f = pure (extract . f . pure)
