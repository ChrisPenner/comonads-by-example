module Comonads.Store.Warehouse where

import qualified Data.Map as M
import Comonads.Store

inventory :: M.Map Int String
inventory =
    M.fromList [ (0, "Fidget spinners")
               , (1, "Books")
               , (2, "Guitars")
               , (3, "Laptops")
               ]

warehouse :: Store Int (Maybe String)
warehouse = store (\shelf -> M.lookup shelf inventory) 0
