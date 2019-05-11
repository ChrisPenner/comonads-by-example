module Comonads.Traced.DepAnalysis where

import qualified Data.Set                   as S
import           Control.Comonad.Traced
import           Comonads.Transformers.Iter

ingredientsOf :: String -> S.Set String
ingredientsOf "string" = S.fromList ["wool"]
ingredientsOf "sticks"  = S.fromList ["wood"]
ingredientsOf "bow"    = S.fromList ["sticks", "string"]
ingredientsOf "arrows"  = S.fromList ["sticks", "feathers", "stone"]
ingredientsOf "quiver" = S.fromList ["arrows", "bow"]
ingredientsOf "torches"  = S.fromList ["coal", "sticks"]
ingredientsOf _        = mempty

recipes :: Traced (S.Set String) (S.Set String)
recipes = traced (foldMap ingredientsOf)

allRequiredIngredientsFor :: S.Set String -> S.Set String
allRequiredIngredientsFor s = trace s $ extendUntilFixedPoint (traces id) recipes
