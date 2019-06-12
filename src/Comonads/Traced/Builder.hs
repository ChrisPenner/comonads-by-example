module Comonads.Traced.Builder where

import Comonads.Traced
import Control.Comonad
import Data.Monoid
import Data.Function ((&))
import qualified Data.Map as M

newBuilder :: Traced [String] String
newBuilder = traced concat

append :: String -> Traced [String] String -> String
append s = trace [s]

logMsg :: Traced [String] String -> String
logMsg = append "hello " =>= append "world"

-- Uh Oh!
-- λ> logMsg newBuilder
-- "worldhello "


betterBuilder :: Traced (Dual [String]) String
betterBuilder = traced (concat . getDual)

betterAppend :: String -> Traced (Dual [String]) String -> String
betterAppend s = trace (Dual [s])

betterLogMsg :: Traced (Dual [String]) String -> String
betterLogMsg = betterAppend "hello " =>= betterAppend "world"


keyedLogger :: Traced (M.Map String String) String
keyedLogger = traced render
  where
    render m = foldMap pairToStr (M.toList m)
    pairToStr (k, v) = k <> ": " <> v <> "\n"

addLog :: String -> String -> Traced (M.Map String String) String -> String
addLog k v = trace (M.singleton k v)

keyedMsg :: String
keyedMsg = keyedLogger & (addLog "userName" "Joe" =>= addLog "msg" "balance not sufficient")
-- msg: balance not sufficient
-- userName: Joe
