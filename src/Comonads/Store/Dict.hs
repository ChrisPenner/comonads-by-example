module Comonads.Store.Dict where

import qualified Data.Map as M
import Comonads.Store

countryPopulation :: Store String (Maybe Int)
countryPopulation = Store (\country -> M.lookup country populations) "Canada"

populations :: M.Map String Int
populations =
    M.fromList [ ("Canada",        37279811)
               , ("Poland",        38028278)
               , ("France",        65480710)
               , ("United States", 329093110)
               , ("Germany",       82438639)
               ]

-- > λ> pos countryPopulation
-- > "Canada"
-- > λ> peek "Poland" countryPopulation
-- > Just 38028278
-- > λ> pos $ seek "Germany" countryPopulation
-- > "Germany"

-- More abstract uses of Store

squared :: Store Int Int
squared = Store (^(2 :: Int)) 10

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


