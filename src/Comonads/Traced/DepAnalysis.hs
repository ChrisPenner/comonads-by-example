module Comonads.Traced.DepAnalysis where

import qualified Data.Set                   as S
import           Comonads.Traced
import           Control.Comonad


ingredientsOf :: String -> S.Set String
ingredientsOf "string"  = S.fromList ["wool"]
ingredientsOf "sticks"  = S.fromList ["wood"]
ingredientsOf "bow"     = S.fromList ["sticks", "string"]
ingredientsOf "arrows"  = S.fromList ["sticks", "feathers", "stone"]
ingredientsOf "quiver"  = S.fromList ["arrows", "bow"]
ingredientsOf "torches" = S.fromList ["coal", "sticks"]
ingredientsOf _         = mempty

recipes :: Traced (S.Set String) (S.Set String)
recipes = traced (foldMap ingredientsOf)

-- | See Comonads.Store.DepAnalysis; it's much nicer
-- allIngredientsFor :: Traced (S.Set String) (S.Set String)
-- allIngredientsFor = extend wfix (selectNext <$> listen recipes)
--   where
--     selectNext :: (S.Set String, S.Set String)
--                -> Traced (S.Set String) (S.Set String)
--                -> S.Set String
--     selectNext (requirements, input) t
--         | S.null (S.difference requirements input) = input
--         | otherwise = trace (S.difference requirements input) t
