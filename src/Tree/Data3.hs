{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Tree.Data3 where

import Data.Semigroup

data Tree a =
    Tree { value    :: a
         , children :: [Tree a]
         }
    deriving (Show, Foldable, Functor)

data SkillNode =
    SN { label          :: String
       , pointsAssigned :: Int
       , totalPoints    :: Int
       }
    deriving (Show)

myTree :: Tree SkillNode
myTree =
    Tree (SN "magic" 2 5)
         [ Tree (SN "fireball" 1 3) [Tree (SN "flamewall" 0 1) []]
         , Tree (SN "levitation" 1 2) []
         ]

pointsRemaining :: Tree SkillNode -> Int
pointsRemaining = getSum . foldMap (Sum . go)
    where
        go sn = totalPoints sn - pointsAssigned sn

tagTree :: Tree SkillNode -> Tree (Int, SkillNode)
tagTree t = Tree (pointsRemaining t, value t) (tagTree <$> children t)

duplicateTree :: Tree a -> Tree (Tree a)
duplicateTree t@(Tree _ children') = Tree t (duplicateTree <$> children')

mapSubtrees :: (Tree a -> b) -> Tree a -> Tree b
mapSubtrees f t = f <$> duplicateTree t

tagTree' :: Tree SkillNode -> Tree (Int, SkillNode)
tagTree' = mapSubtrees go
    where
        go t = (pointsRemaining t, value t)
