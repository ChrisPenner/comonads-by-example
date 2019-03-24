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

tagDistances :: [Tetromino] -> [(Tetromino, Maybe Int)]
tagDistances [] = []
tagDistances xs@(x:rest) = (x, distanceToHero xs) : tagDistances rest

duplicate' :: [a] -> [[a]]
duplicate' [] = []
duplicate' xs@(_:rest) = xs : duplicate' rest

tagDistances' :: [Tetromino] -> [(Tetromino, Maybe Int)]
tagDistances' xs = pair <$> duplicate' xs
  where
    pair [] = error "hrmmmm?"
    pair ys@(y:_) = (y, distanceToHero ys)

balloon :: ([a] -> b) -> [a] -> [b]
balloon f = fmap f . duplicate'


tagDistances'' :: [Tetromino] -> [(Tetromino, Maybe Int)]
tagDistances'' = balloon pair
  where
    pair :: [Tetromino] -> (Tetromino, Maybe Int)
    pair xs = (head xs, distanceToHero xs)

tagDistances''' :: NonEmpty Tetromino -> NonEmpty (Tetromino, Maybe Int)
tagDistances''' = extend $ \xs -> (extract xs, distanceToHero $ toList xs)
