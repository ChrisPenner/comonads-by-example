{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
module Comonads.Cofree where

import Control.Comonad

data Cofree f a = a :< f (Cofree f a)
    deriving (Functor)

-- QuantifiedConstraints allows us to avoid using "Show1" and other messiness
deriving instance Show a => (forall x. Show x => (Show (f x))) => Show (Cofree f a)

instance (Functor f) => Comonad (Cofree f) where
  extract :: Cofree f a -> a
  extract (a :< _) = a
  duplicate :: Cofree f a -> Cofree f (Cofree f a)
  duplicate w@(_ :< rest) = w :< fmap duplicate rest
