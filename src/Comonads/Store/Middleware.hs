{-# LANGUAGE OverloadedStrings #-}
module Comonads.Store.Middleware where

import qualified Data.Text as T
import Comonads.Store
import Control.Comonad
import Data.Monoid
import Data.Map as M

logger :: Store T.Text T.Text
logger = store id ""

flattenNewlines :: Store T.Text T.Text -> T.Text
flattenNewlines = T.replace "\n" ";" . extract

format :: Store T.Text T.Text -> T.Text
format = T.unlines . fmap ("> " <>) . T.lines . extract


redactPasswords :: Store T.Text T.Text -> T.Text
redactPasswords w | T.isPrefixOf "password" (pos w) = "<REDACTED>"
                  | otherwise = extract w


completeLogger :: Store T.Text T.Text
completeLogger = logger =>> format =>> redactPasswords

type Validator = Store T.Text (Either T.Text T.Text)
type Check = Validator -> Either T.Text T.Text
validator :: Validator
validator = store Right ""

addPepper :: T.Text -> Validator -> Either T.Text T.Text
addPepper pepper = fmap (<> pepper) . extract

longerThan :: Int -> Check
longerThan n w | T.length (pos w) > n = extract w
               | otherwise = Left "too short"

hasSpecialChar :: Check
hasSpecialChar w
    | T.any (`elem` ['@', '$', '%', '!']) (pos w) = extract w
    | otherwise = Left "no special chars"

hash :: Check
hash w = extract w >>= pure . T.reverse


validatePass :: T.Text -> Either T.Text T.Text
validatePass t = peek t $ validator =>> addPepper "PEPPA" =>> hash =>>  longerThan 3 =>> hasSpecialChar


reducer :: Store T.Text T.Text
reducer = store id ""

reduceParens :: Store T.Text T.Text -> T.Text
reduceParens w =
    let next = T.replace "()" ""  . extract $ w
     in if next == pos w then next
                         else peek next w

scoreDoubles :: Store [Int] (Sum Int) -> Sum Int
scoreDoubles w = countPoints $ pos w
  where
    calcPoints = M.unionsWith (+) . fmap (\x -> M.singleton x x)
    countPoints = foldMap countsToPoints . calcPoints
    countsToPoints 2 = Sum 2
    countsToPoints 3 = Sum 6
    countsToPoints 4 = Sum 12
    countsToPoints _ = Sum 0


scoreDoubles :: Store [Int] (Sum Int) -> Sum Int
scoreDoubles w = countPoints $ pos w
  where
    calcPoints = M.unionsWith (+) . fmap (\x -> M.singleton x x)
    countPoints = foldMap countsToPoints . calcPoints
    countsToPoints 2 = Sum 2
    countsToPoints 3 = Sum 6
    countsToPoints 4 = Sum 12
    countsToPoints _ = Sum 0
