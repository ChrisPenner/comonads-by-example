{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Comonads.Cofree.Examples where

import Comonads.Cofree
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Compose
import System.Directory
import Control.Applicative
import qualified Data.List.NonEmpty as NE


type Env e a = Cofree (Const e) a
type Store s a = Cofree (Compose ((,) s) ((->) s)) a
type Traced m a = Cofree ((->) m) a -- When `m` is a Monoid


type Stream a = Cofree Identity a
count :: Stream Int
count = 0 :< Identity (fmap (+1) count)

count' :: Cofree Identity Int
count' = coiter coalg 0
  where
    coalg :: Int -> Identity Int
    coalg n = Identity (n + 1)

---

type NonEmpty a = Cofree Maybe a

alphabet :: NonEmpty Char
alphabet = coiter maybeNext 'a'
  where
    maybeNext :: Char -> Maybe Char
    maybeNext 'z' = Nothing
    maybeNext a = Just $ succ a

---

type Tree a = Cofree [] a

simpleTree :: Cofree [] Char
simpleTree = 'a' :< [ 'b' :< ['c' :< []]
                    , 'd' :< []
                    ]

fileTree :: IO (Tree FilePath)
fileTree = unfoldM crawl "."
  where
    crawl :: FilePath -> IO (FilePath, [FilePath])
    crawl path = (,) path <$> listDirectory' path
    -- Just return an empty list if reading children fails,
    -- or if it's a file not a dir.
    listDirectory' :: FilePath -> IO [FilePath]
    listDirectory' path =
        listDirectory path <|> pure []

---

data Pair a = Pair a a
    deriving (Show, Eq, Functor, Foldable, Traversable)

type Zipper a = Cofree (Compose Pair Maybe) a

moveLeft :: Zipper Int -> Maybe (Zipper Int)
moveLeft (_ :< Compose (Pair l _)) = l

moveRight :: Zipper Int -> Maybe (Zipper Int)
moveRight (_ :< Compose (Pair _ r)) = r

zipper :: Zipper Int
zipper = unfold move ([-1, -2], 0, [1, 2])
  where
    move :: ([Int], Int, [Int]) -> (Int, Compose Pair Maybe ([Int], Int, [Int]))
    move z@(_, focus, _) = (focus, Compose $ Pair (toLeft z) (toRight z))
    toRight (ls, focus, r:rs) = Just (focus:ls, r, rs)
    toRight (_, _, []) = Nothing
    toLeft (l:ls, focus, rs) = Just (ls, l, focus:rs)
    toLeft ([], _, _) = Nothing

type BinTree a = Cofree (Compose Pair Maybe) a

numbers :: NE.NonEmpty Int
numbers = 33 NE.:| [11, 44, 55, 22]

binTree :: BinTree Int
binTree = unfold split numbers
  where
    split :: NE.NonEmpty Int -> (Int, Compose Pair Maybe (NE.NonEmpty Int))
    split (x NE.:| rest) = (x, Compose $ Pair (NE.nonEmpty $ filter (<x) rest) (NE.nonEmpty $ filter (>x) rest))
