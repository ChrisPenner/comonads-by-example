{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Comonads.Sheet where

import Control.Comonad
import Comonads.Store
-- import Data.Stream.Infinite
import Debug.Trace
import Data.Bifunctor (first)
import Data.Char
import Data.List

getCells :: Functor f => f s -> Store s a -> f a
getCells = experiment . const

-- The following data represents this spreadsheet:
--   | A    |  B     | C
-- 0 | 1.00 |  7     | 7.00
-- 1 | 0.75 |  5     | 3.75
-- 2 | 2.00 |  9     | 18.00
-- 3 | ---- | ------ | -----
-- 4 | Tax  |  Total | Total With Tax
-- 5 | 0.15 |  28.75 | 33.06

dataDef :: (Char, Int) -> Double
dataDef ('B', 2) = 1
dataDef ('B', 3) = 0.75
dataDef ('B', 4) = 2

dataDef ('C', 2) = 7
dataDef ('C', 3) = 5
dataDef ('C', 4) = 9
dataDef _ = 0

sheet :: Store (Char, Int) Double
sheet = (store (dataDef . first toUpper ) ('A', 1))

printSheet' :: (Show a) => Store (Char, Int) a ->  IO ()
printSheet' w = printSheet w ('D', 5)

printSheet :: (Show a) => Store (Char, Int) a -> (Char, Int) -> IO ()
printSheet w (endCol, endRow) = putStrLn . unlines $ do
    rowI <- [0 .. endRow]
    return . (padded rowI <>) . intercalate " | " $
      if rowI == 0
        then fmap padded $ ['A'..toUpper endCol]
        else do colI <- ['A'.. toUpper endCol]
                let item = peek (colI, rowI) w
                return $ padded item
  where
    padded s = show s <> replicate (5 - length (show s)) ' '

dataDef2 :: (Char, Int) -> Store (Char, Int) Double ->  Double
dataDef2 ('A', 0) _ = 1
dataDef2 ('A', 1) _ = 0.75
dataDef2 ('A',  2) _ = 2

dataDef2 ('B',  0) _ = 7
dataDef2 ('B',  1) _ = 5
dataDef2 ('B',  2) _ = 9

dataDef2 ('C',  row) w | row < 5 =
  let price = peek ('A', row) w
      quant = peek ('B', row) w
   in price * quant

-- Tax
dataDef2 ('A', 5) _ = 0.15
-- Total
dataDef2 ('B', 5) w = sum . getCells (('C',) <$> [0..2]) $ w
-- Total With Tax
dataDef2 ('C', 5) w =
    let tax = peek ('A', 5) w
        total = peek ('B', 5) w
     in (tax * total) + total

dataDef2 _ _ = 0

sheet2 :: Store (Char, Int) Double
sheet2 = extend wfix (store dataDef2 ('A', 0))

-- λ> peek Total sheet
-- 28.75
-- λ> peek Tax sheet
-- 0.15
-- λ> peek TotalWithTax sheet
-- 33.0625
