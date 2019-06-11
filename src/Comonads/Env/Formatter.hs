module Comonads.Env.Formatter where

import Comonads.Env
import Control.Comonad

data Settings =
    Settings
    { padAmount :: Int
    , maxLength :: Int
    , padChar :: Char
    } deriving (Show)

getPadChar :: Env Settings a -> Char
getPadChar w = asks padChar w

setPadChar :: Char -> Settings -> Settings
setPadChar c s = s{padChar=c}

trunc :: Env Settings [a] -> [a]
trunc w =
    let mxLngth = asks maxLength w
    in take mxLngth (extract w)

pad :: Env Settings String -> String
pad w =
    let padAmt = asks padAmount w
        c = asks padChar w
     in replicate padAmt c <> extract w <> replicate padAmt c

-- Showing some other notations
pad2 :: Env Settings String -> String
pad2 = do
  padding <- replicate <$> asks padAmount <*> asks padChar
  txt     <- extract
  return $ padding <> txt <> padding

-- Showing some other notations
pad3 :: Env Settings String -> String
pad3 = do
  let padding = replicate <$> asks padAmount <*> asks padChar
  padding <> extract <> padding

-- λ> pipeline context
-- "***Hello***"
pipeline :: Env Settings String -> String
pipeline = trunc =>= pad

-- Order matters
-- λ> pipeline2 context
-- "***He"
pipeline2 :: Env Settings String -> String
pipeline2 = pad =>= trunc

-- Local allows us to change settings WITHIN the pipeline
-- λ> pipeline3 context
-- "***___Hello___***"
pipeline3 :: Env Settings String -> String
pipeline3 = trunc =>= pad . local (setPadChar '_') =>= pad

-- We can configure our environment here
context :: Env Settings String
context = env (Settings {padAmount=3, maxLength=5, padChar='*'}) "Hello World"
