module Comonads.Traced.Config where

import Data.Monoid
import Control.Comonad
import Comonads.Traced

data Config =
    Config
    { files :: [FilePath]
    , port  :: Int
    , site :: String
    , admin :: String
    } deriving (Show)

type ConfMod = Endo Config

addFilePath :: FilePath -> ConfMod
addFilePath f = Endo $ \c ->
    c{ files = (f : files c)
     }

setPort :: Int -> ConfMod
setPort p = Endo $ \c -> c
    { port = p
    }

setSSL :: ConfMod
setSSL = Endo $ \c -> c{site= "https://" ++ site c}

editConfig :: Traced ConfMod Config -> Traced ConfMod Config
editConfig t =
    t
    =>> trace (setPort 8080)
    =>> trace (addFilePath "~/.conf")
    =>> trace (addFilePath "./conf")

buildConfig :: Traced ConfMod Config -> Config
buildConfig = extract
