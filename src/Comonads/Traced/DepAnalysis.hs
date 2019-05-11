module Comonads.Traced.DepAnalysis where

import qualified Data.Set                   as S
import           Control.Comonad.Traced
import           Comonads.Transformers.Iter

ingredientsOf :: String -> S.Set String
ingredientsOf "string" = S.fromList ["wool"]
ingredientsOf "stick"  = S.fromList ["wood"]
ingredientsOf "bow"    = S.fromList ["stick", "string"]
ingredientsOf "arrow"  = S.fromList ["stick", "feather", "stone"]
ingredientsOf "quiver" = S.fromList ["arrow", "bow"]
ingredientsOf "torch"  = S.fromList ["coal", "stick"]
ingredientsOf _        = mempty

recipes :: Traced (S.Set String) (S.Set String)
recipes = traced (foldMap ingredientsOf)

allRequiredIngredientsFor :: S.Set String -> S.Set String
allRequiredIngredientsFor s = trace s $ extendUntilFixedPoint (traces id) recipes
