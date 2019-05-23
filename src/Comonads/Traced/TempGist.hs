{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Comonads.Traced.TempGist where

import Control.Comonad (Comonad (..), (=>>))
import Data.Semigroup (Any (..))
import Text.Pretty.Simple (pPrint)


data Settings = Settings
    { settingsHasLibrary :: Any
    , settingsGitHub     :: Any
    , settingsTravis     :: Any
    } deriving (Show)

instance Semigroup Settings where
    Settings a1 b1 c1 <> Settings a2 b2 c2 =
        Settings (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid Settings where
    mempty = Settings mempty mempty mempty

data Project = Project
    { projectName       :: !String
    , projectHasLibrary :: !Bool
    , projectGitHub     :: !Bool
    , projectTravis     :: !Bool
    } deriving (Show)

type ProjectBuilder = Settings -> Project

buildProject :: String -> ProjectBuilder
buildProject projectName Settings{..} = Project
    { projectHasLibrary = getAny settingsHasLibrary
    , projectGitHub     = getAny settingsGitHub
    , projectTravis     = getAny settingsTravis
    , ..
    }

hasLibraryB :: ProjectBuilder -> Project
hasLibraryB builder = builder $ mempty { settingsHasLibrary = Any True }

gitHubB :: ProjectBuilder -> Project
gitHubB builder = builder $ mempty { settingsGitHub = Any True }

alwaysTravisB :: ProjectBuilder -> Project
alwaysTravisB builder = builder $ mempty { settingsTravis = Any True }

travisB :: ProjectBuilder -> Project
travisB builder =
  let project = extract builder
  in project { projectTravis = projectGitHub project }


main :: IO ()
main = do
    -- plain
    pPrint $ extract $ buildProject "minimal-project"
    pPrint $ extract $ buildProject "only-library" =>> hasLibraryB
    pPrint $ extract $ buildProject "library-github" =>> hasLibraryB =>> gitHubB

    -- dependent: 1 level
    pPrint $ extract $ buildProject "travis" =>> travisB
    pPrint $ extract $ buildProject "always-travis" =>> alwaysTravisB
    pPrint $ extract $ buildProject "github-travis" =>> gitHubB =>> travisB
    pPrint $ extract $ buildProject "travis-github" =>> travisB =>> gitHubB
