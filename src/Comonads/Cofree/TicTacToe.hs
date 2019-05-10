{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
module Comonads.Cofree.TicTacToe where

-- import Control.Comonad
-- import Comonads.Cofree
-- import Control.Comonad.Cofree
-- import Data.Functor.Compose
-- import Data.Grid
-- import Control.Lens

-- type Board = Grid [3, 3]

-- data Player = X | O
--     deriving (Eq, Show)

-- type BoardState = Board (Maybe Player)
-- step :: (Player, BoardState) -> (Board `Compose` Maybe) (Player, BoardState)
-- step (p, bs) = tabulate go
--   where
--     nextPlayer =
--       case p of
--         X -> O
--         O -> X
--     go c = bs & cell c .~ (nextPlayer, Compose (bs & cell c .~ p))

-- possibleMoves :: Cofree (Board `Compose` Maybe) (Player, BoardState)
-- possibleMoves = coiter step (X, start)
--   where
--     start :: BoardState
--     start =
--         Board
--           Nothing Nothing Nothing
--           Nothing Nothing Nothing
--           Nothing Nothing Nothing
