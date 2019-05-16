module Comonads.Store.DepAnalysis where

import qualified Data.Set                   as S
import           Comonads.Transformers.Iter
import           Comonads.Dynamic.Cofree
import           Control.Comonad.Store

ingredientsOf :: String -> S.Set String
ingredientsOf "string" = S.fromList ["wool"]
ingredientsOf "sticks"  = S.fromList ["wood"]
ingredientsOf "bow"    = S.fromList ["sticks", "string"]
ingredientsOf "arrows"  = S.fromList ["sticks", "feathers", "stone"]
ingredientsOf "quiver" = S.fromList ["arrows", "bow"]
ingredientsOf "torches"  = S.fromList ["coal", "sticks"]
ingredientsOf _        = mempty

recipeStore :: Store String (S.Set String)
recipeStore = store ingredientsOf ""

allDepsStore :: Store String (S.Set String)
allDepsStore = dynFix' go recipeStore
  where
    go :: (S.Set String, Store String (S.Set String)) -> (S.Set String)
    go (deps, _) | S.null deps = mempty
    go (deps, rec) = deps <> foldMap (flip peek rec) deps


allDepsOf :: String -> S.Set String
allDepsOf s = dynWFix go (seek s recipeStore)
  where
    go :: (S.Set String, Store String (S.Set String)) -> (S.Set String)
    go (deps, _) | S.null deps = mempty
    go (deps, rec) = deps <> foldMap (flip peek rec) deps
