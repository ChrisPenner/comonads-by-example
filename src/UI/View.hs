{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module UI.View (initApp, AppState, scroll, nextTab, prevTab) where

import Brick
-- import Brick.Widgets.Core
import Data.Functor.Day
import qualified Data.Text as T
import Control.Comonad.Traced
-- import Control.Comonad.Store
import Comonads.Zipper

type AppState = Window (Widget String)

-- class (forall l r. Comonad (w l r)) => BiComonad w where
--   extendL :: (w (f l) (g r) a -> l') -> w (f l) (g r) a -> w (f l') (g r) a
--   extendR :: (w (f l) (g r') a -> r') -> w (f l) (g r) a -> w (f l) (g r') a
--   extractL :: w (f l) (g r) a -> l
--   extractR :: w (f l) (g r) a -> r
--   duplicateL :: w (f l) (g r) a -> w (f (f l)) (g r) a
--   duplicateR :: w (f l) (g r) a -> w (f l) (g (g r)) a

txt1 :: T.Text
txt1 = "Thing one\nThing two\nThing three\nThing four"

txt2 :: T.Text
txt2 = "Guvat bar\nGuvat gjb\nGuvat guerr\nGuvat sbhe"

-- type ItemList = Store Int
-- itemList :: ItemList T.Text
-- itemList = undefined

liftDay :: (a -> b -> c) -> f a -> g b -> Day f g c
liftDay h f g = Day f g h

extendL :: Comonad f => (forall a. f a -> a) -> Day f g c -> Day f g c
extendL f = trans1 (extend f)

-- extendR :: Comonad g => (forall a. g a -> a) -> Day f g c -> Day f g c
-- extendR f = trans2 (extend f)

type ScrollPos = Traced (Sum Int)

type Tabs = Zipper
type Window a = Day ScrollPos Tabs a

nextTab :: AppState -> AppState
nextTab = trans2 moveRight'
prevTab :: AppState -> AppState
prevTab = trans2 moveLeft'

scroll :: Int -> AppState -> AppState
scroll n = extendL (trace (Sum n))

buildTabs :: ScrollPos Int -> Tabs T.Text -> Window (Widget String)
buildTabs = liftDay buildText
  where
    buildText offset txt' = txt . T.unlines . drop (fromIntegral offset) $ T.lines txt'

initApp :: AppState
initApp = buildTabs scrollPos tabs
  where
    tabs :: Tabs T.Text
    tabs = fromList [txt1, txt2]
    scrollPos :: Traced (Sum Int) Int
    scrollPos = traced getSum
