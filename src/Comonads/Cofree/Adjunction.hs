{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Comonads.Cofree.Adjunction where

import Control.Comonad
import Data.Functor.Adjunction
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.Functor.Rep
import Data.Distributive

data Dir a = L a | R a
    deriving (Show, Functor, Foldable, Traversable)

data Branch a = a :<>: a
    deriving (Show, Functor, Foldable, Traversable)

instance Distributive Branch where
  distribute = distributeRep

instance Representable Branch where
  type Rep Branch = Dir ()
  index  (l :<>: _) L{} = l
  index  (_ :<>: r) R{} = r
  tabulate f = f (L ()) :<>: f (R ())

instance Adjunction Dir Branch where
  unit a = L a :<>: R a
  counit (L (l :<>: _)) = l
  counit (R (_ :<>: r)) = r

left, right :: Free Dir ()
left = liftF $ L ()
right = liftF $ R ()

game :: Int -> Cofree Branch Ordering
game n = flip compare n <$> coiter coalg 0
  where
    coalg :: Int -> Branch Int
    coalg x = (x - 1) :<>: (x + 1)

basicGame :: Cofree Branch Ordering
basicGame = game 3

explore :: Free Dir ()
explore = do
    right
    right
    right

runGame :: Free Dir () -> IO ()
runGame = print . indexAdjunction basicGame

mkGame :: Int -> Cofree Branch Int
mkGame = leftAdjunct (iter alg)
  where
    alg :: Dir Int -> Int
    alg (L n) = n - 1
    alg (R n) = n + 1

-------

data Outcomes a =
    Outcomes
    { debitO    :: (Double -> a)
    , creditO   :: (Double -> a)
    , interestO :: (Double -> a)
    } deriving (Functor)

data Events a
    = Debit Double a
    | Credit Double a
    | Interest Double a
    deriving (Functor)

instance Distributive Outcomes where
  distribute = distributeRep

instance Representable Outcomes where
  type Rep Outcomes = Events ()
  index = indexAdjunction
  tabulate = tabulateAdjunction

instance Adjunction Events Outcomes where
  unit a =
      Outcomes
        { debitO    = \amt -> Debit amt a
        , creditO   = \amt -> Credit amt a
        , interestO = \amt -> Interest amt a
        }
  counit (Debit d p) = debitO p d
  counit (Credit d p) = creditO p d
  counit (Interest d p) = interestO p d


-- This runs everything in reverse :|
-- accountCo :: Double -> Cofree Outcomes Double
-- accountCo = leftAdjunct (iter alg)
--   where
--     alg :: Events Double -> Double
--     alg (Debit debitAmt balance) = balance - debitAmt
--     alg (Credit creditAmt balance) = creditAmt + balance
--     alg (Interest interestRate balance) = balance + (balance * interestRate)

runTransactions :: Free Events Double -> Double
runTransactions = rightAdjunct (coiter exec)
  where
    exec :: Double -> Outcomes Double
    exec balance =
      Outcomes
        { debitO    = \d -> balance - d
        , creditO   = \c -> balance + c
        , interestO = \rate -> balance + balance * rate
        }

credit :: Double -> Free Events ()
credit amt = liftF $ Credit amt ()

debit :: Double -> Free Events ()
debit amt = liftF $ Debit amt ()

interest :: Double -> Free Events ()
interest percent = liftF $ Interest percent ()

transactions :: Free Events ()
transactions = do
    credit 1000
    interest 0.1
    debit 50

-- runTransactions :: Double -> Free Events () -> Double
-- runTransactions balance = indexAdjunction (accountCo balance)

----

instance (Adjunction f u, Applicative f) => Applicative (Adj u f) where
  pure = Adj . unit
  Adj a <*> Adj b = Adj $ mzipWithRep (<*>) a b
    -- where
        -- Not lawful, but close :|
        -- go a' b' = extractL a' <$> b'

newtype Adj f u a = Adj {runAdj :: f (u a)}
  deriving (Show, Functor)

instance (Adjunction f u) => Comonad (Adj f u) where
  extract = counit . runAdj
  duplicate (Adj fua) = Adj ((fmap Adj) . unit <$> fua)
