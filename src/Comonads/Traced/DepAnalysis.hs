module Comonads.Traced.DepAnalysis where

import qualified Data.Set                   as S
-- import           Comonads.Traced
import           Comonads.Transformers.Iter
import           Control.Comonad.Traced
import           Control.Comonad.Trans.Traced (listen, listens, censor)

ingredientsOf :: String -> S.Set String
ingredientsOf "string" = S.fromList ["wool"]
ingredientsOf "stick" = S.fromList ["wood"]
ingredientsOf "bow" = S.fromList ["stick", "string"]
ingredientsOf "arrow" = S.fromList ["stick", "feather", "stone"]
ingredientsOf "quiver" = S.fromList ["string", "arrow", "bow"]
ingredientsOf _ = mempty

recipes :: Traced (S.Set String) (S.Set String)
recipes = traced $ foldMap ingredientsOf

allIngredients :: Traced (S.Set String) (S.Set String)
allIngredients = extend (traces id) recipes

allRequiredIngredientsFor :: String -> S.Set String
allRequiredIngredientsFor s = trace (S.singleton s) $ extendUntilFixedPoint (traces id) recipes
