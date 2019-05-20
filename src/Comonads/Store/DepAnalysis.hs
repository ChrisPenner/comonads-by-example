module Comonads.Store.DepAnalysis where

import qualified Data.Set                   as S
import           Control.Comonad.Store

ingredientsOf :: String -> S.Set String
ingredientsOf "string" = S.fromList ["wool"]
ingredientsOf "sticks"  = S.fromList ["wood"]
ingredientsOf "bow"    = S.fromList ["sticks", "string"]
ingredientsOf "arrows"  = S.fromList ["sticks", "feathers", "stone"]
ingredientsOf "quiver" = S.fromList ["arrows", "bow"]
ingredientsOf "torches"  = S.fromList ["coal", "sticks"]
ingredientsOf _        = mempty

recipeStore :: Store (S.Set String) (S.Set String)
recipeStore = store (foldMap ingredientsOf) mempty

allDepsStore :: Store (S.Set String) (S.Set String)
allDepsStore = extend wfix (go <$> recipeStore)
  where
    go :: S.Set String -> Store (S.Set String) (S.Set String) -> (S.Set String)
    go deps _ | S.null deps = mempty
    go deps rec = deps <> peek deps rec
