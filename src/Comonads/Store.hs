{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NumericUnderscores #-}

module Comonads.Store where

import qualified Data.Map as M

import Control.Comonad

data Store s a = Store (s -> a) s
    deriving Functor

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) =
      Store (\s' -> Store f s') s
  extend g store = g <$> duplicate store

pos :: Store s a -> s
pos (Store _ s) = s

peek :: s -> Store s a -> a
peek s (Store f _) = f s

peeks :: (s -> s) -> Store s a -> a
peeks g (Store f s) = f (g s)

seek :: s -> Store s a -> Store s a
seek s (Store f _) = Store f s

seeks :: (s -> s) -> Store s a -> Store s a
seeks g (Store f s) = Store f (g s)

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment search (Store f s) = f <$> search s

--- Examples

-- Using store as a Map

countryPopulation :: Store String (Maybe Int)
countryPopulation = Store (\country -> M.lookup country populations) "Canada"
  where
    populations =
        M.fromList [ ("Canada",        37_279_811)
                   , ("Poland",        38_028_278)
                   , ("France",        65_480_710)
                   , ("United States", 329_093_110)
                   , ("Germany",       82_438_639)
                   ]

-- > λ> pos countryPopulation
-- > "Canada"
-- > λ> peek "Poland" countryPopulation
-- > Just 38028278
-- > λ> pos $ seek "Germany" countryPopulation
-- > "Germany"

-- More abstract uses of Store

squared :: Store Int Int
squared = Store (^(2 :: Int)) 10

-- > λ> pos squared
-- > 10
-- > λ> extract squared
-- > 100
-- > λ> peek 2 squared
-- > 4
-- > λ> extract $ seeks (+1) squared
-- > 121
-- > λ> experiment (\n -> [n + 10, n + 20, n + 30]) squared
-- > [400,900,1600]
