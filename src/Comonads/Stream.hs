{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFoldable #-}
module Comonads.Stream where

import Control.Comonad
import Data.Foldable
import Data.List (intercalate)

data Stream a = a :> Stream a
    deriving (Functor, Foldable)

instance (Show a) => Show (Stream a) where
  show s = intercalate " :> " (show <$> takeS 5 s) ++ " :> ..."

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

ix' :: Int -> Stream a -> a
ix' n = extract . dropS n

dropS :: Int -> Stream a -> Stream a
dropS n = extend (ix n)

takeS :: Int -> Stream a -> [a]
takeS n = take n . toList

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
rollingAvg windowSize = extend (windowedAvg windowSize)

windowedAvg :: Int -> Stream Int -> Double
windowedAvg windowSize s = avg (takeS windowSize s)

avg :: [Int] -> Double
avg xs =
        fromIntegral (sum xs)
    / fromIntegral (length xs)

countStream :: Stream Int
countStream = fromList [1..]

negStream :: Stream Int
negStream = fromList [2, -3, -5, 6, 3, -10, 3]

-- subNegative:: Stream Int -> Stream Int
-- λ> randomStream
-- 2 :> -3 :> -5 :> 6 :> 3 :> -10 :> 3 :> 2 :> -3 :> -5 :> ...
-- λ> subNegative randomStream
-- 2 :> 6 :> 6 :> 6 :> 3 :> 3 :> 3 :> 2 :> 6 :> 6 :> ...
subNegative :: Stream Int -> Stream Int
subNegative = extend upcast
  where
    upcast :: Stream Int -> Int
    upcast s = head . dropWhile (<0) $ toList s
