module Comonads.Transformers.ReportBuilder where

import Control.Comonad
import Control.Comonad.Store
import Control.Comonad.Env
import Control.Comonad.Traced
import Data.Functor.Compose
import Control.Arrow

data ReportStyle = Detailed | Summary

data Region = America | UK | Germany
    deriving (Show, Eq, Ord)

projections :: Region -> Sum Int ->  Double
projections UK      (Sum month) = 1.2 ^ (max 0 month) * 100
projections America (Sum month) = 1.3 ^ (max 0 month) * 200
projections Germany (Sum month) = 1.5 ^ (max 0 month) * 300

monthlyReport :: EnvT  ReportStyle (TracedT (Sum Int) (Store Region)) Double
monthlyReport = (EnvT Detailed (TracedT (store projections UK)))

reportPipeline :: (ComonadTraced (Sum Int) w, ComonadStore Region w, ComonadEnv ReportStyle w) => w Double -> String
reportPipeline = buildReport =>= addHeader

fullComparison :: (ComonadTraced (Sum Int) w, ComonadStore Region w, ComonadEnv ReportStyle w) => w Double -> String
fullComparison = do
    ourReport <- buildReport
    comparisons <- comparisonReport
    return $ ourReport <> "\n" <> replicate 40 '=' <> "\n" <> comparisons

currencyOf :: Region -> String
currencyOf America = "$"
currencyOf UK = "£"
currencyOf Germany = "€"

buildReport :: (ComonadTraced (Sum Int) w, ComonadStore Region w, ComonadEnv ReportStyle w) => w Double -> String
buildReport = do
    region <- pos
    salesAmt <- extract
    prev <- trace (Sum (-1))
    next <- trace (Sum 1)
    style <- ask
    pure $ case style of
        Summary  -> show region <> ": did well with " <> currencyOf region <> show salesAmt
        Detailed -> "This months report for "
            <> show region
            <> " shows that they made some sort of sales as shown by the total "
            <> currencyOf region
            <> show salesAmt
            <> "\nPrevious month's sales: " <> currencyOf region <> show prev
            <> "\nNext month's projections: " <> currencyOf region <> show next


addHeader :: (ComonadStore Region w, ComonadEnv ReportStyle w) => w String -> String
addHeader = do
    style <- ask
    region <- pos
    report <- extract
    pure $ case style of
            Detailed -> "Please find enclosed your DETAILED report for " <> show region <> ": \n" <> report
            Summary -> "Please find enclosed your SUMMARY report: " <> show region <> ": \n" <> report

comparisonReport :: (ComonadTraced (Sum Int) w, ComonadStore Region w, ComonadEnv ReportStyle w) => w Double -> String
comparisonReport = do
    others <- buildReport =>= compareToOthers
    currentRegion <- pos
    return $ show currentRegion <> ": Comparison Report\n" <> unlines others

compareToOthers :: (ComonadStore Region w) => w a -> [a]
compareToOthers = experiment (\r -> filter (/= r) allRegions)

compareToOthers' :: (ComonadStore Region w) => w a -> [(Region, a)]
compareToOthers' = getCompose . experiment (\r -> Compose $ (id &&& id) <$> filter (/= r) allRegions)

allRegions :: [Region]
allRegions = [UK, America, Germany]
