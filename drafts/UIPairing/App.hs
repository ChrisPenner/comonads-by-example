{-# LANGUAGE OverloadedStrings #-}
module UIPairing.App where

import           Brick
import           Graphics.Vty.Attributes as V
import           Graphics.Vty            as V
import           Control.Monad
import           UI.View
import           Control.Comonad

type Event = ()
type ResourceName = String

app :: App AppState () ResourceName
app =
    App
    { appDraw = appDraw'
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = appHandleEvent'
    , appStartEvent = return
    , appAttrMap = appAttrMap'
    }

appAttrMap' :: AppState -> AttrMap
appAttrMap' _ = attrMap V.defAttr []

appHandleEvent' :: AppState
                -> BrickEvent ResourceName ()
                -> EventM ResourceName (Next AppState)
appHandleEvent' s (VtyEvent (EvKey KUp _)) = continue $ scroll (-1) s
appHandleEvent' s (VtyEvent (EvKey KDown _)) = continue $ scroll 1 s
appHandleEvent' s (VtyEvent (EvKey KRight _)) = continue $ nextTab s
appHandleEvent' s (VtyEvent (EvKey KLeft _)) = continue $ prevTab s
appHandleEvent' s _ = halt s

appDraw' ::  AppState -> [Widget String]
appDraw' s = [img]
  where
    img = extract s


run :: IO AppState
run = do
    let initV = V.mkVty V.defaultConfig
    v <- initV
    customMain v (return v) Nothing app initApp

main :: IO ()
main = void run
