module Comonads.Traced.Prepend where

import Control.Comonad
import Comonads.Traced
import Data.Char

exclamation :: Traced String String
exclamation = traced (\s -> toUpper <$> s <> "!!")

-- λ> trace "hello" exclamation
-- "HELLO!!"
-- λ> extract $ exclamation =>> trace "hello"
-- "HELLO!!"
-- λ> extract $ exclamation =>> trace "jerry" =>> trace " " =>> trace "hello"
-- "HELLO JERRY!!"
