{-# LANGUAGE TypeOperators #-}
module UIPairing.Pair where

import Control.Comonad.Cofree
import Control.Comonad.Store
import Control.Monad.Free as Free
import Control.Monad.State
import Data.Functor.Compose
import Data.Functor.Day
import Data.Bifunctor
import Data.Foldable
import Debug.Trace

import System.IO.Unsafe

type AppState = (Int, String)

type MyApp s a = Day (Free ((,) s) `Compose` IO) (StateT s IO) a

data Event = Up | Down

up :: Free ((,) Event) ()
up = liftF (Up, ())

down :: Free ((,) Event) ()
down = liftF (Down, ())

eventHandler :: Event -> State AppState ()
eventHandler Up  = modify (first (+1))
eventHandler Down  = modify (first (subtract 1))

type AppType s a = Day (Cofree (Store s)) (Free (State s)) a
type View = String

renderer :: Cofree (Store AppState) View
renderer = coiterW duplicate (store (trace "HIT!" . uncurry drop) (0, ['a'..'z']))

myApp :: AppType AppState View
myApp = Day renderer (Pure ()) const

render :: Cofree (Store AppState) View -> Free ((,) Event) () -> View
render (v :< _) (Pure _) = v
render (_ :< store') (Free (e, next)) =
    let state' = eventHandler e
    in render (peeks (execState state') store') next

actions :: Free ((,) Event) ()
actions = up >> up >> down

-- buildMutator :: IO (Free (State AppState) ())
-- buildMutator = do
--     l <- getLine
--     recurse <- unsafeInterleaveIO buildMutator
--     case l of
--         ('+':_) -> do
--             pure $ Free (modify (first (+1)) $> recurse)
--         ('-':_) -> do
--             pure $ Free (modify (first (subtract 1)) $> recurse)
--         ('q':_) -> pure $ Pure ()
--         _ -> pure . Free $ pure recurse

getEvent :: IO (Maybe (Free ((,) Event) ()))
getEvent = do
    l <- getLine
    pure $ case l of
        ('+' : _) -> Just up
        ('-' : _) -> Just down
        _         -> Nothing

runApp :: IO ()
runApp = loop (Pure ())
    where
      loop es = do
        mEvt <- getEvent
        let es' = case mEvt of
                    Nothing -> case es of
                        Pure () -> Pure ()
                        Free (_, x) -> x
                    Just e -> es >> e
        let v = render renderer es'
        print v
        loop es'
