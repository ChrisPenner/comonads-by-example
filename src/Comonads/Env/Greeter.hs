module Comonads.Env.Greeter where

import Comonads.Env
import Control.Comonad

data Person = Person {firstName :: String, lastName :: String}

joe :: Person
joe = Person "Joe" "Bob"

joeEnv :: Env Person Int
joeEnv = env joe 26

printAge :: Env Person Int -> String
printAge w =
  let firstName' = asks firstName w
      lastName' = asks lastName w
      age = extract w
   in firstName' <> " " <> lastName' <> " is " <> show age

printAgeDoNotation :: Env Person Int -> String
printAgeDoNotation = do
    firstName' <- asks firstName
    lastName' <- asks lastName
    age <- extract
    return $ firstName' <> " " <> lastName' <> " is " <> show age
