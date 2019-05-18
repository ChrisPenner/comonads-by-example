module Comonads.Store.Squared where

import Comonads.Store

squared :: Store Int Int
squared = Store (\x -> x^(2 :: Int)) 10

-- > λ> pos squared
-- > 10
-- > λ> extract squared
-- > 100
-- > λ> peek 2 squared
-- > 4
-- > λ> extract $ seeks (+1) squared
-- > 121
-- > λ> experiment (\n -> [n + 10, n + 20, n + 30]) squared
-- > [400,900,1600]


