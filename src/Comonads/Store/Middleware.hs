{-# LANGUAGE OverloadedStrings #-}
module Comonads.Store.Middleware where

import qualified Data.Text           as T
import           Comonads.Store
import           Control.Comonad

-- Precompose
normalizePath :: Store T.Text T.Text -> T.Text
normalizePath = peeks cleaned
  where
    cleaned url = T.strip . T.toLower $ url

-- Postcompose
collapseSlashes :: Store T.Text T.Text -> T.Text
collapseSlashes = T.replace "//" "/" . extract

-- Wrapping transform
showTransformation :: Store T.Text T.Text -> T.Text
showTransformation w = pos w <> ": " <> extract w

urlTransformer :: Store T.Text T.Text
urlTransformer = store id "" =>> collapseSlashes =>> normalizePath =>> showTransformation

badUrl :: T.Text
badUrl = " gOOgLe.com//a/B//c  "

