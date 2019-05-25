module Comonads.Transformers.Examples where

import Control.Comonad
import Control.Comonad.Store
import Control.Comonad.Env
import Data.List.NonEmpty
import qualified Data.Map as M

-- data AST = Str String
-- freeVariables :: StoreT String (Env VarType) AST

type AccountID = String
type UserID = String
type AccountBalance = Int
data AccountType = Chequing | Savings
    deriving (Show, Eq)

-- bank :: StoreT AccountID (Env AccountType) AccountBalance
-- bank = StoreT () "ACCOUNT-123456"

-- Layered user/account structure
bank :: StoreT AccountType (Store UserID) AccountBalance
bank = StoreT go Chequing
  where
    go :: Store UserID (AccountType -> AccountBalance)
    go = store (\usrID actType -> selectAccount actType $ M.findWithDefault (0, 0) usrID accounts) ""
    selectAccount Chequing (amount, _) = amount
    selectAccount Savings  (_, amount) = amount

accounts :: M.Map UserID (AccountBalance, AccountBalance)
accounts = M.fromList [("Joe", (30, 50))]
