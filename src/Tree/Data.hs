{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Tree.Data where

import Data.Semigroup

data Tree a =
    Tree { root :: a
         , children :: [Tree a]
         } deriving (Show, Eq, Functor, Foldable, Traversable)

data SkillNode =
    SN { skillName       :: String
              , pointsSpent     :: Int
              , pointsAvailable :: Int
              } deriving (Eq)

instance Show SkillNode where
    show sn = skillName sn <> " (" <> show (pointsSpent sn) <> "/" <> show (pointsAvailable sn) <> ")"

getPointsLeft :: SkillNode -> Int
getPointsLeft sn = pointsAvailable sn - pointsSpent sn

magic, fireball, flamewall, levitation :: SkillNode
magic = SN "Magic" 2 5
fireball = SN "Fireball" 1 3
flamewall = SN "Flamewall" 0 1
levitation = SN "Levitation" 1 2

sampleTree :: Tree SkillNode
sampleTree = Tree magic [ Tree fireball [Tree flamewall []]
                        , Tree levitation []
                        ]


countRemainingPoints :: Tree SkillNode -> Int
countRemainingPoints t = getSum $ foldMap (Sum . getPointsLeft) t

annotateNodes :: Tree SkillNode -> Tree (Int, SkillNode)
annotateNodes t@(Tree root' children') = Tree (countRemainingPoints t, root') (annotateNodes <$> children')

expand :: Tree a -> Tree (Tree a)
expand t@(Tree _ children') = Tree t (expand <$> children')

annotateNodes' :: Tree SkillNode -> Tree (Int, SkillNode)
annotateNodes' t = fmap go . expand $ t
    where
        go t' = (countRemainingPoints t, root t')

overSubtree :: (Tree a -> b) -> Tree a -> Tree b
overSubtree f = fmap f . expand
