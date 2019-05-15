module Comonads.Env.Greeter where

import Comonads.Env
import Control.Comonad

data Person = Person {name :: String, age :: Int}

joe :: Person
joe = Person "Joe" 26

joeEnv :: Env Person String
joeEnv = env joe "Hi"

greet :: Env Person String -> String
greet = do
    greeting <- extract
    name'    <- asks name
    return $ greeting ++ " " ++ name'
