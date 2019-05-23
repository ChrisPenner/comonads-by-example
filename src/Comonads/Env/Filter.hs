module Comonads.Env.Filter where

import Control.Comonad
import Comonads.Env

vowels :: String
vowels = "aAeEiIoOuU"

filtered :: Env String String -> String
filtered w = let invalid = ask w
              in filter (not . (`elem` invalid)) $ extract w

append :: String -> Env e String -> String
append s w = extract w ++ s

main :: IO ()
main = do
    putStrLn . extract $ env vowels "Hello World" =>> append "!!" =>> filtered
    -- Hll Wrld!!
    putStrLn . extract $ env vowels "Hello World" =>> filtered . local (++ ['A'..'Z'])
    -- ll rld
