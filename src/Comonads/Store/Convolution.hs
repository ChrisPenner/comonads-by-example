module Comonads.Store.Convolution where

import Comonads.Store
import Control.Comonad
import Data.Maybe
import Control.Applicative
import Data.Monoid

image :: [[Double]]
image = [ [3, 5, 8]
        , [4, 5, 9]
        , [2, 1, 7]
        ]

image2 :: [[Double]]
image2 = [ [3, 3, 3]
         , [3, 3, 3]
         , [3, 3, 3]
         ]


(!?) :: [a] -> Int -> Maybe a
xs !? i
    | i < 0 || i >= length xs = Nothing
    | otherwise = Just (xs !! i)

mkStore :: (Num a) => [[a]] -> Store (Sum Int, Sum Int) a
mkStore img = store (fromMaybe 0 . get) (0, 0)
  where
    get (Sum x, Sum y) = do
        row <- img !? fromIntegral x
        row !? fromIntegral y


imageStore :: Store (Sum Int, Sum Int) Double
imageStore = store (fromMaybe 0 . get) (0, 0)
  where
    get (Sum x, Sum y) = do
        row <- image !? x
        row !? y

drawImage :: (Show a) => Int -> Store (Sum Int, Sum Int) a -> String
drawImage size g = unlines $ do
    x <- [0..size-1]
    let ls = do y <- [0..size-1]
                (show $ peek (Sum x, Sum y) g) <> " "
    return ls

printImage :: (Show a) => Int -> Store (Sum Int, Sum Int) a -> IO ()
printImage size g = putStrLn $ drawImage size g

neighbours :: Num i => (i, i) -> [(i, i)]
neighbours (x, y) = liftA2 (,) [x - 1, x, x + 1] [y - 1, y, y + 1]

gauss1 :: Store (Sum Int, Sum Int) Double -> Double
gauss1 w = let context = experiment neighbours w
            in sum context / fromIntegral (length context)

gauss :: Store (Sum Int, Sum Int) Double -> Store (Sum Int, Sum Int) Double
gauss = extend gauss1

peaks1 :: Ord a => Store (Sum Int, Sum Int) a -> a
peaks1 w = let context = experiment neighbours w
            in maximum context

peaks :: Store (Sum Int, Sum Int) Double -> Store (Sum Int, Sum Int) Double
peaks = extend peaks1

sobel1 :: Floating a => Store (Sum Int, Sum Int) a -> a
sobel1 w = sqrt $ (v ** 2) + (h ** 2)
    where
        v = sum . experiment neighbours $ liftW2 (*) w sobelVertical
        h = sum . experiment neighbours $ liftW2 (*) w sobelHorizontal

sobel :: Store (Sum Int, Sum Int) Double -> Store (Sum Int, Sum Int) Double
sobel = extend sobel1

sobelHorizontal :: Floating a => Store (Sum Int, Sum Int) a
sobelHorizontal = mkStore
    [ [-1, -2, -1]
    , [0, 0, 0]
    , [1, 2, 1]
    ]

sobelVertical :: Floating a => Store (Sum Int, Sum Int) a
sobelVertical = mkStore
    [ [-1, 0, 1]
    , [-2, 0, 2]
    , [-1, 0, 1]
    ]

