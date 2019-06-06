{-# LANGUAGE OverloadedStrings #-}
module Comonads.Store.Middleware where

import qualified Data.Text           as T
import           Comonads.Store
import           Control.Comonad
import           Control.Applicative

flattenNewlines :: Store T.Text T.Text -> T.Text
flattenNewlines = T.replace "\n" ";" . extract

format :: Store T.Text T.Text -> T.Text
format = T.unlines . fmap ("> " <>) . T.lines . extract


redactPasswords :: Store T.Text T.Text -> T.Text
redactPasswords w | T.isPrefixOf "password" (pos w) = "<REDACTED>"
                  | otherwise = extract w


type Validator = Store T.Text (Either T.Text T.Text)
type Check = Validator -> Either T.Text T.Text
validator :: Validator
validator = store Right ""

addPepper :: T.Text -> Validator -> Either T.Text T.Text
addPepper pepper = fmap (<> pepper) . extract

longerThan :: Int -> Check
longerThan n w | T.length (pos w) > n = extract w
               | otherwise = Left "too short"

hasSpecialChar :: Check
hasSpecialChar w
    | T.any (`elem` ['@', '$', '%', '!']) (pos w) = extract w
    | otherwise = Left "no special chars"

hash :: Check
hash w = extract w >>= pure . T.reverse


validatePass :: T.Text -> Either T.Text T.Text
validatePass t = peek t $ validator =>> addPepper "PEPPA" =>> hash =>>  longerThan 3 =>> hasSpecialChar


splitter :: Store T.Text [T.Text]
splitter = store T.words ""

limitLength :: Store T.Text a -> Maybe a
limitLength w | T.length (pos w) > 10 = Nothing
              | otherwise = Just $ extract w

-- post composition
cleanEmpty :: Store a [T.Text] -> [T.Text]
cleanEmpty = filter (not . T.null) . extract

-- preComposition
trim :: Store T.Text a -> a
trim w = peek (T.strip (pos w)) w

-- examples
chained :: Store T.Text (Maybe [T.Text])
chained = splitter =>> limitLength =>> trim

chained' :: Store T.Text (Maybe [T.Text])
chained' = splitter =>> trim =>> limitLength

data Req = Req {url :: T.Text, reqBody :: T.Text}
  deriving (Show)
data Resp = Resp {respBody :: T.Text}
  deriving (Show)

req :: Req
req = Req "https://GooGle.ca/search " "{}"

type Middleware = Store Req (IO Resp) -> IO Resp
type App = Store Req (IO Resp)

simpleServer :: App
simpleServer = store run (Req "" "")
  where
    run _ = pure $ Resp "200 OK"

brokenServer :: App
brokenServer = store (const $ fail "no good") (Req "" "")

-- Precompose
normalizePath :: Middleware
normalizePath = peeks stripDoubles
  where
    stripDoubles (Req url' body') = Req (T.strip . T.toLower $ url') body'

-- Postcompose
gzip :: Middleware
gzip w = extract w >>= pure . Resp . T.take 5 . respBody

-- Wrap
logger :: Middleware
logger w = do
    putStrLn $ "REQUEST: " <> show (pos w)
    resp <- extract w
    putStrLn $ "RESPONSE: " <> show resp
    pure resp

retry :: Middleware
retry w = extract w <|> extract w <|> pure (Resp "Internal Server error")

app :: App
app = brokenServer =>> logger =>> normalizePath
