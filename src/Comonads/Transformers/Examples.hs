module Comonads.Transformers.Examples where

import Control.Comonad
import Control.Comonad.Store
import Control.Comonad.Env
import qualified Data.Map as M
import Data.Maybe

data Region = America | UK | Germany
    deriving (Show, Eq, Ord)

data ReportStyle = Detailed | Summary

stats :: M.Map Region Int
stats = M.fromList [ ( America, 24), (Germany, 36), (UK, 12)
                   ]
statsStore :: EnvT ReportStyle (Store Region) Int
statsStore = EnvT Detailed (store (\key -> fromMaybe 0 $ M.lookup key stats) UK)

mkReport :: (ComonadStore Region w, ComonadEnv ReportStyle w) => w Int -> w String
mkReport data' = data' =>> buildReport =>> addHeader

currencyOf :: Region -> String
currencyOf America = "$"
currencyOf UK = "£"
currencyOf Germany = "€"

buildReport :: (ComonadStore Region w, ComonadEnv ReportStyle w) => w Int -> String
buildReport = do
    style <- ask
    region <- pos
    salesAmt <- extract
    pure $ case style of
        Summary  -> show region <> ": did well with " <> currencyOf region <> show salesAmt
        Detailed -> "This years report for "
            <> show region
            <> " shows that they made some sort of sales as shown by the total "
            <> currencyOf region
            <> show salesAmt


addHeader :: (ComonadStore Region w, ComonadEnv ReportStyle w) => w String -> String
addHeader = do
    style <- ask
    report <- extract
    pure $ case style of
            Detailed -> "Please find enclosed your DETAILED report: \n" <> report
            Summary -> "Please find enclosed your SUMMARY report: \n" <> report
