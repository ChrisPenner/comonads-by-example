module Comonads.Cofree.Examples where

import Comonads.Cofree
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Compose

type Stream a = Cofree Identity a
type NonEmpty a = Cofree Maybe a
type Tree a = Cofree [] a

type Env e a = Cofree (Const e) a
type Store s a = Cofree (Compose ((,) s) ((->) s)) a
type Traced m a = Cofree ((->) m) a -- When `m` is a Monoid
