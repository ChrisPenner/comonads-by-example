{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Comonads.Env.BankAccount where

import Control.Comonad
import Control.Comonad.Env

data Currency = Dollars | Euro | Pounds
  deriving (Show, Eq)

data Account a =
    Account { balance :: a
            , owner :: String
            , currency :: Currency
            , overdraft :: Bool
            , interestRate :: Double
            } deriving (Show, Eq, Functor)

instance Comonad Account where
  duplicate a = a $> a
  extract = balance

instance ComonadEnv (Account ()) Account where
  ask account = account $> ()

joe'sChqing :: Account Double
joe'sChqing = Account 35 "Joe" Dollars True 0.1

deposit :: Double -> Account Double -> Double
deposit n = (+n) . extract

applyInterest :: Account Double -> Double
applyInterest = do
    rate <- asks interestRate
    amt <- extract
    return $ amt + (amt * rate)

showReport :: Account Double -> String
showReport = do
    currency' <- asks currency
    amt <- extract
    owner' <- asks owner
    return $ owner' <> " has " <> show amt <> " " <> show currency'

withdraw :: Double -> Account Double -> Maybe Double
withdraw n = do
    current <- extract
    hasOverdraft <- asks overdraft
    if n > current && not (hasOverdraft)
        then return Nothing
        else return $ Just (current - n)

transactions :: Account Double -> String
transactions = deposit 5 =>= deposit 10 =>= applyInterest =>= showReport
