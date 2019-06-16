module Comonads.Transformers.RegionReport where

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Traced
import Control.Comonad.Store
import Data.Function ((&))

data ReportStyle = Detailed | Summary

data Region = America | UK | Germany
    deriving (Show, Eq, Ord)

projections :: Region -> Sum Int ->  Double
projections UK      (Sum month) = 1.2 ^ (max 0 month) * 100
projections America (Sum month) = 1.3 ^ (max 0 month) * 200
projections Germany (Sum month) = 1.5 ^ (max 0 month) * 300

reportConfig  :: EnvT  ReportStyle (TracedT (Sum Int) (Store Region)) Double
reportConfig  = (EnvT Detailed (TracedT (store projections UK)))

previousMonth :: ComonadTraced (Sum Int) w => w a -> a
previousMonth = trace (Sum (-1))

nextMonth :: ComonadTraced (Sum Int) w => w a -> a
nextMonth = trace (Sum 1)

detailedReport :: (ComonadTraced (Sum Int) w, ComonadStore Region w) => w Double -> String
detailedReport = do
    salesAmt <- extract
    prev <- previousMonth
    next <- nextMonth
    region <- pos
    return $ unlines [ show region <> ":"
                     , "This months sales in totality are: " <> show salesAmt
                     , "Previous month's sales: " <> show prev
                     , "Next month's projections: " <> show next
                     ]

buildHeader :: (ComonadEnv ReportStyle w) => w a-> String
buildHeader = do
    style <- ask
    return $ case style of
            Detailed -> "Please find enclosed your DETAILED report\n"
            Summary -> "Please find enclosed your SUMMARY report\n"

buildReport :: (ComonadTraced (Sum Int) w, ComonadEnv ReportStyle w, ComonadStore Region w) => w Double -> String
buildReport = do
    header <- buildHeader
    salesAmt <- extract
    style <- ask
    case style of
        Summary -> return $ header <> "We achieved " <> show salesAmt <> " in sales!"
        Detailed -> do
            rpt <- detailedReport
            compReport <- comparisonReport
            return $ header <> rpt <> "\n" <> compReport


detailedReport' :: Int -> String
detailedReport' month = reportConfig =>> buildReport & trace (Sum month)

summaryReport :: Int -> String
summaryReport month = reportConfig =>> buildReport . local (const Summary) & trace (Sum month)

otherRegions :: (ComonadStore Region w) => w a -> [a]
otherRegions = experiment (\r -> filter (/= r) allRegions)

allRegions :: [Region]
allRegions = [UK, America, Germany]

comparisonReport :: (ComonadTraced (Sum Int) w, ComonadStore Region w) => w Double -> String
comparisonReport = do
    otherReports <- detailedReport =>= otherRegions
    return $ "Comparison Report\n" <> unlines otherReports
