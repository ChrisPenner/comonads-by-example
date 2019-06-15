{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
module Comonads.Sheet where

import Control.Comonad
import Comonads.Store

data CellRef
    = Price Int
    | Quant Int
    | Cost Int
    | Tax
    | Total
    | TotalWithTax
    deriving (Show, Eq)

getCells :: Functor f => f s -> Store s a -> f a
getCells = experiment . const

-- The following data represents this spreadsheet:
--   | Price   Quant   Cost
-- 0 | 1.00    7       7.00
-- 1 | 0.75    5       3.75
-- 2 | 2.00    9       18.00
--
--   Tax    Total   Total With Tax
--  0.15    28.75   33.06

dataDef :: CellRef -> Store CellRef Double ->  Double
dataDef (Price 0) = pure 1
dataDef (Price 1) = pure 0.75
dataDef (Price 2) = pure 2

dataDef (Quant 0) = pure 7
dataDef (Quant 1) = pure 5
dataDef (Quant 2) = pure 9

dataDef (Cost r) = do
    price <- peek (Price r)
    quant <- peek (Quant r)
    return $ price * quant

dataDef Tax = pure 0.15
dataDef Total = sum . getCells (Cost <$> [0 .. 2])
dataDef TotalWithTax = do
    tax' <- peek Tax
    total' <- peek Total
    pure $ (tax' * total') + total'

dataDef _ = pure 0

sheet :: Store CellRef Double
sheet = extend wfix (store dataDef Total)

-- λ> peek Total sheet
-- 28.75
-- λ> peek Tax sheet
-- 0.15
-- λ> peek TotalWithTax sheet
-- 33.0625
