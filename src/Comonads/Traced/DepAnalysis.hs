module Comonads.Traced.DepAnalysis where

import qualified Data.Map        as M
import qualified Data.Set        as S
import           Comonads.Traced
import           Data.Maybe

recipes :: M.Map String (S.Set String)
recipes = M.fromList [ ("torch", S.fromList ["coal", "wood"])
                     , ("fence", S.fromList ["stone", "wood"])
                     , ("lamp", S.fromList ["torch", "iron"])
                     , ("terrace", S.fromList ["lamp", "deck"])
                     , ("deck", S.fromList ["wood"])
                     ]

findIngredients :: Traced (S.Set String) (S.Set String)
findIngredients = Traced $ foldMap lookupSingle
  where
    lookupSingle s = fromMaybe mempty (M.lookup s recipes)


