{-# LANGUAGE DeriveFunctor #-}
module Comonads.Traced where

import Control.Comonad

newtype Traced m a = Traced { runTraced :: m -> a }
    deriving Functor

instance (Monoid m) => Comonad (Traced m) where
  extract (Traced f) = f mempty
  duplicate (Traced f) =
      Traced $ \m -> Traced (f . mappend m)
  extend g = fmap g . duplicate

traced :: (m -> a) -> Traced m a
traced  = Traced

trace :: m -> Traced m a -> a
trace m (Traced f) = f m

traces :: Monoid m => (a -> m) -> Traced m a -> a
traces f t = trace (f (extract t)) t

listen :: Traced m a -> Traced m (a, m)
listen (Traced f) = Traced $ \m -> (f m, m)
