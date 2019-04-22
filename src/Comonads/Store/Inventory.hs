module Comonads.Store.Inventory where

import Comonads.Store
import Control.Comonad
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

type Inventory = Store String Int

ingredients :: String -> S.Set String
ingredients "chair" = S.fromList ["wood"]
ingredients "bow" = S.fromList ["wood", "string"]
ingredients "bucket" = S.fromList ["copper"]
ingredients "alloy" = S.fromList ["copper", "steel"]
ingredients x = S.fromList [x]

catalog :: Store String Int
catalog = Store (\s -> fromMaybe 0 $ M.lookup s inventory) "rock"
  where
    inventory :: M.Map String Int
    inventory = M.fromList [("wood", 10), ("copper", 2), ("string", 50)]

canBuild :: Store String Int -> Store String Bool
canBuild w = w =>> checkIngredients =>> getAll . foldMap (All . (>0)) . extract

checkIngredients :: Store String Int -> M.Map String Int
checkIngredients = experiment buildMap
  where
    buildMap :: String -> M.Map String String
    buildMap = M.fromSet id . ingredients
