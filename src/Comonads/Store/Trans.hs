module Comonads.Store.Trans where

import Control.Comonad.Store


f :: Int -> Int -> (Int, Int)
f x y = (x, y)

xStore :: Store Int (Int -> (Int, Int))
xStore = store f 0

grid :: StoreT Int (Store Int) (Int, Int)
grid = StoreT xStore 0

-- λ> extract $  (TS.seek 10) grid
-- (0,10)
-- λ> extract $ cohoist (TS.seek 10) grid
-- (10,0)
-- λ> TS.pos . lower $  cohoist (TS.seek 10) grid
-- 10
