module Comonads.Store.Conway where

import Control.Comonad
import Comonads.Store
import Data.Monoid

type Coord = (Sum Int, Sum Int)
type Grid = Store Coord Bool

startingGrid :: Grid
startingGrid = Store (const False) (0, 0)

checkCell :: (Sum Int, Sum Int) -> Grid -> Bool
checkCell = peek

neighbourLocations :: [Coord]
neighbourLocations =
    [ (-1, -1), (0, -1), (1, -1)
    , (-1,  0),          (1,  0)
    , (-1,  1), (0,  1), (1,  1)
    ]

computeLiveness :: Grid -> Bool
computeLiveness grid = (alive && numLivingNeighbours == 2) || numLivingNeighbours == 3
  where
    alive :: Bool
    alive = extract grid
    numLivingNeighbours :: Int
    numLivingNeighbours = length . filter id $ experiment findNeighbours grid
    findNeighbours :: Coord -> [Coord]
    findNeighbours c = mappend c <$> neighbourLocations

step :: Grid -> Grid
step = extend computeLiveness

drawGrid :: Grid -> Int -> String
drawGrid g size = unlines $ do
    x <- [0..size]
    return $ do
        y <- [0..size]
        return . toChar $ checkCell (Sum x, Sum y) g
  where
    toChar True  = '#'
    toChar False = '.'


addCell :: Coord -> Grid -> Bool
addCell c g = extract g || pos g == c

basicGrid :: Grid
basicGrid = startingGrid
        =>> addCell (1, 1)
        =>> addCell (1, 2)
        =>> addCell (1, 3)
        =>> peeks (mappend (-3, -3))
