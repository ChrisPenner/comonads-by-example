{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Comonads.Store.ConwayExercise where

import Comonads.Store
import qualified Data.Set as S
import Data.Monoid
import Comonads.Store.Conway ()

type Coord = (Sum Int, Sum Int)
type Grid = Store Coord Bool

-- | The starting state of the grid
startingGrid :: Grid
startingGrid = store checkAlive (0, 0)
  where
    checkAlive :: Coord -> Bool
    checkAlive coord = S.member coord livingCells

    livingCells :: S.Set Coord
    livingCells = S.fromList [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

-- | A cell is alive in the next iteration IFF:
-- * The cell is currently ALIVE AND it has 2 living neighbours
-- OR
-- * The cell has exactly 3 living neighbours
-- OTHERWISE the cell is dead in the next iteration
computeCellLiveness :: Grid -> Bool
computeCellLiveness _grid = undefined
  where
    _currentCellAlive :: Bool
    _currentCellAlive = undefined
    _neighboursAlive :: _Neighbours Bool
    _neighboursAlive = undefined
    _numLivingNeighbours :: Int
    _numLivingNeighbours = undefined

-- | Iterate the game of life by one step
step :: Grid -> Grid
step = undefined
