{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFoldable #-}
module Comonads.Zipper.Trans where

import qualified Data.List.NonEmpty as NE
import Data.List
import Control.Comonad
import Data.Maybe
import Control.Comonad.Env
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Control.Arrow

data ZipperT w a = ZipperT [w a] (w a) [w a]
    deriving (Functor, Foldable, Show, Eq)

moveLeft :: ZipperT w a -> Maybe (ZipperT w a)
moveLeft (ZipperT (l:ls) c rs) = Just $ ZipperT ls l (c:rs)
moveLeft _ = Nothing

moveRight' :: ZipperT w a -> ZipperT w a
moveRight' z = fromMaybe z $ moveRight z

moveLeft' :: ZipperT w a -> ZipperT w a
moveLeft' z = fromMaybe z $ moveLeft z

moveRight :: ZipperT w a -> Maybe (ZipperT w a)
moveRight (ZipperT ls c (r:rs)) = Just $ ZipperT (c:ls) r rs
moveRight _ = Nothing

instance (Comonad w) => Comonad (ZipperT w) where
  extract (ZipperT _ c _)= extract c
  duplicate :: ZipperT w a -> ZipperT w (ZipperT w a)
  duplicate z@(ZipperT ls w rs) = ZipperT ls' (w $> z) rs'
    where
      ls' = zipWith ($>) ls $ unfoldr (fmap pair . moveLeft) z
      rs' = zipWith ($>) rs $ unfoldr (fmap pair . moveRight) z
      pair :: a -> (a, a)
      pair a = (a, a)

instance ComonadTrans ZipperT where
  lower (ZipperT _ z _) = z

instance (ComonadEnv e w) => ComonadEnv e (ZipperT w) where
  ask = ask . lower

instance ComonadHoist ZipperT where
  cohoist f (ZipperT ls c rs) = ZipperT (f <$> ls) (f c) (f <$> rs)

indexedZipper :: NE.NonEmpty a -> ZipperT (Env Int) a
indexedZipper (x NE.:| xs) = ZipperT [] (env 0 x) (zipWith env [1 ..] xs)

indexedAlpha :: ZipperT (Env Int) Char
indexedAlpha = indexedZipper ('a' NE.:| ['b'..'z'])

mapEven :: (a -> a) -> ZipperT (Env Int) a -> ZipperT (Env Int) a
mapEven f = extend go
  where
    go w | asks even w = f $ extract w
         | otherwise = extract w

withIndex :: ComonadEnv e w => ZipperT w a -> ZipperT w (e, a)
withIndex = extend (ask &&& extract)
