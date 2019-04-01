{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

{-# LANGUAGE DeriveTraversable #-}

module Tree.Data2 where

import           Data.Semigroup

data Tree a =
    Tree { value    :: a
         , children :: [Tree a]
         }
    deriving (Show, Eq, Functor, Foldable, Traversable)

data SkillNode =
    SN { label           :: String
       , pointsSpent     :: Int
       , availablePoints :: Int
       }
    deriving (Show, Eq)

example :: Tree SkillNode
example =
    Tree (SN "magic" 2 5)
         [ Tree (SN "fireball" 1 3) [Tree (SN "flamewall" 0 1) []]
         , Tree (SN "levitation" 1 2) []
         ]

sumAvailablePoints :: Tree SkillNode -> Int
sumAvailablePoints = getSum . foldMap (Sum . sumNode)
  where
    sumNode sn = availablePoints sn - pointsSpent sn

tagTree :: Tree SkillNode -> Tree (Int, SkillNode)
tagTree t@(Tree sn cs) = Tree (sumAvailablePoints t, sn) (fmap tagTree cs)

duplicate :: Tree a -> Tree (Tree a)
duplicate t@(Tree _ children') = Tree t (duplicate <$> children')

mapSubtrees :: (Tree a -> b) -> Tree a -> Tree b
mapSubtrees f = fmap f . duplicate

tagTree' :: Tree SkillNode -> Tree (Int, SkillNode)
tagTree' = mapSubtrees go
    where
        go t = (sumAvailablePoints t, value t)
