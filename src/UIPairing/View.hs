{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module UIPairing.View (initApp, AppState) where

import Brick
import Data.Functor.Day
import qualified Data.Text as T
import Control.Comonad.Traced
import Comonads.Zipper

type AppState = (T.Text, Int)

txt1 :: T.Text
txt1 = "Thing one\nThing two\nThing three\nThing four"

initApp :: AppState
initApp = (txt1, 0)
