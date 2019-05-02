{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
module Comonads.Stream where

import Control.Comonad

data Stream a = a :> Stream a
    deriving Functor

instance Comonad Stream where
  extract :: Stream a -> a
  extract (a :> _) = a
  duplicate :: Stream a -> Stream (Stream a)
  duplicate s@(_ :> rest) = s :> duplicate rest
  extend :: (Stream a -> b) -> Stream a -> Stream b
  extend f s@(_ :> rest) = f s :> extend f rest

ix :: Int -> Stream a -> a
ix n _ | n < 0 = error "don't do that silly"
ix 0 (a :> _) = a
ix n (_ :> rest) = ix (n - 1) rest

dropS :: Int -> Stream a -> Stream a
dropS n = extend (ix n)

takeS :: Int -> Stream a -> [a]
takeS n _ | n < 0 = error "don't do that silly"
takeS 0 _ = []
takeS n (a :> rest) = a : takeS (n - 1) rest

windows :: Int -> Stream a -> Stream [a]
windows n = extend (takeS n)

cycleS :: a -> Stream a
cycleS a = a :> cycleS a

fromList :: [a] -> Stream a
fromList xs = go (cycle xs)
  where
    go [] = error "don't do that silly"
    go (a:rest) = a :> go rest

rollingAvg :: Int -> Stream Int -> Stream Double
rollingAvg windowSize = extend (avg . takeS windowSize)
  where
    avg :: [Int] -> Double
    avg xs =
          fromIntegral (sum xs)
        / fromIntegral (length xs)

countStream :: Stream Int
countStream = fromList [1..]
