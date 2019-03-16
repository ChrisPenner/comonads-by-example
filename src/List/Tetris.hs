module List.Tetris where

import Data.List.NonEmpty hiding (head)
import Data.List
import Control.Comonad

data Tetromino = Ricky | Island | Hero | Teewee | Smashboy
    deriving (Eq, Show)

upNext :: [Tetromino]
upNext = Ricky : Smashboy : Hero : []

distanceToHero :: [Tetromino] -> Maybe Int
distanceToHero = findIndex (== Hero)

pairDistances :: [Tetromino] -> [(Tetromino, Maybe Int)]
pairDistances [] = []
pairDistances xs@(x:rest) = (x, distanceToHero xs) : pairDistances rest

duplicate' :: [a] -> [[a]]
duplicate' [] = []
duplicate' xs@(_:rest) = xs : duplicate' rest

pairDistances' :: [Tetromino] -> [(Tetromino, Maybe Int)]
pairDistances' xs = pair <$> duplicate' xs
  where
    pair [] = error "hrmmmm?"
    pair ys@(y:_) = (y, distanceToHero ys)

balloon :: ([a] -> b) -> [a] -> [b]
balloon f = fmap f . duplicate'


pairDistances'' :: [Tetromino] -> [(Tetromino, Maybe Int)]
pairDistances'' = balloon pair
  where
    pair :: [Tetromino] -> (Tetromino, Maybe Int)
    pair xs = (head xs, distanceToHero xs)

pairDistances''' :: NonEmpty Tetromino -> NonEmpty (Tetromino, Maybe Int)
pairDistances''' = extend $ \xs -> (extract xs, distanceToHero $ toList xs)
