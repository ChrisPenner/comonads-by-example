module Comonads.Cofree.Trans where

import Control.Comonad.Trans.Cofree
import Control.Comonad.Env
import Data.List.NonEmpty as NE
import Data.Char

type IndexedTree f i a = CofreeT f (Env i) a
type IndexedList i a = CofreeT Maybe (Env i) a

seedList :: NE.NonEmpty Char
seedList = NE.fromList ['a'..'z']

-- Build a List where each element has its index inside the Env comonad
myList :: IndexedList Int Char
myList = extract <$> coiterT coalg start
  where
    coalg :: Env Int (NonEmpty Char) -> Maybe (Env Int (NonEmpty Char))
    coalg w
        | i <- ask w, (_ :| (x : xs)) <- extract w = Just (env (i + 1) (x :| xs))
        | otherwise = Nothing

    start :: Env Int (NonEmpty Char)
    start = env 0 (seedList)

-- Check if our index is even, if it is, then uppercase the char there
toUpperEven :: IndexedList Int Char -> IndexedList Int Char
toUpperEven = extend go
  where
    go w
        | even (ask w) = toUpper (extract w)
        | otherwise = extract w

-- Map over all indices matching the predicate
overIndices :: Functor f => (i -> Bool)
            -> (a -> a)
            -> IndexedTree f i a
            -> IndexedTree f i a
overIndices p f = extend go
  where
    go w
        | p (ask w) = f (extract w)
        | otherwise = extract w

-- Map over a particular index
overIndex :: (Eq i, Functor f) => i -> (a -> a) -> IndexedTree f i a -> IndexedTree f i a
overIndex x = overIndices (== x)
