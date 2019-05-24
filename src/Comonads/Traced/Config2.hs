{-# LANGUAGE NamedFieldPuns #-}
module Comonads.Traced.Config2 where

import Control.Comonad
import Data.Semigroup
import Text.Pretty.Simple

data Config = Config
    { hasTravis :: Any
    , hasGithub :: Any
    , hasLib :: Any
    } deriving (Show, Eq)

data Project = Project
    { name :: String
    , useTravis :: Bool
    , useGithub :: Bool
    , useLib :: Bool
    } deriving (Show, Eq)

instance Semigroup Config where
  Config a b c <> Config a' b' c' = Config (a <> a') (b <> b') (c <> c')

instance Monoid Config where
  mempty = Config mempty mempty mempty

type ProjectBuilder = Config -> Project

addLib :: ProjectBuilder -> Project
addLib f = f $ mempty{hasLib=Any True}

addGithub :: ProjectBuilder -> Project
addGithub f = f $ mempty{hasGithub=Any True}

alwaysTravis :: ProjectBuilder -> Project
alwaysTravis f = f $ mempty{hasTravis=Any True}

addTravis :: ProjectBuilder -> Project
addTravis builder =
    let project = extract builder
    in project { useTravis = useGithub project }

build :: String -> ProjectBuilder
build name Config{hasTravis, hasGithub, hasLib} = Project{name, useTravis=getAny hasTravis, useGithub=getAny hasGithub, useLib=getAny hasLib}

main2 :: IO ()
main2 = do
    pPrint $ extract $ build "travis" =>> addTravis
    pPrint $ extract $ build "always-travis" =>> alwaysTravis
    pPrint $ extract $ build  "github-travis" =>> addGithub =>> addTravis
    pPrint $ extract $ build  "travis-github"=>> addTravis =>> addGithub
