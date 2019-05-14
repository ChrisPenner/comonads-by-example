{-# LANGUAGE TypeOperators #-}
module Comonads.Cofree.FileTree where

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Cofree
import Control.Monad.Free
import qualified Control.Monad.Trans.Free as FF
import Control.Arrow
import Data.Traversable
import Data.Functor.Compose
import Data.Functor.Foldable
import Control.Applicative
import System.Directory
import qualified Data.Map as M

type FileTreeIO = Cofree (IO `Compose` M.Map FilePath) [FilePath]
type FileTreeC  = Free (Env [FilePath] `Compose` (M.Map FilePath)) FileTreeIO

mkFileTree :: FilePath -> FileTreeC
mkFileTree path = Pure $ coiter coalg [path]
  where
    coalg :: [FilePath] -> (IO `Compose` M.Map FilePath) [FilePath]
    coalg paths = Compose $ traverse (\p -> listDirectory p <|> pure []) (toMap paths)

explored :: FileTreeC -> [FilePath]
explored = iter alg . fmap (const [])
  where
    alg w = ask $ getCompose w

toMap :: Ord a => [a] -> M.Map a a
toMap = M.fromList . fmap (id &&& id)

cwd :: FileTreeC
cwd = mkFileTree "."

-- deeper :: FileTreeC -> IO FileTreeC
-- deeper = sequenceA . (>>= go)
--   where
--     go :: FileTreeIO -> Free (Env [FilePath] `Compose` (M.Map FilePath))
--     go (_ :< Compose ioNext) =

    -- go
    --       :: FF.FreeF
    --            (Compose (Env [FilePath]) (M.Map FilePath))
    --            (Cofree (Compose IO (M.Map FilePath)) [FilePath])
    --            (IO a)
    --          -> IO
    --               (FF.FreeF
    --                  (Compose (Env [FilePath]) (M.Map FilePath))
    --                  (Cofree (Compose IO (M.Map FilePath)) [FilePath])
    --                  a)
    -- go (FF.Pure (_ :< Compose ionext)) = do
        -- mapNext <- ionext
        -- pure $ FF.Free (Compose $ env (foldMap extract mapNext) mapNext)

deeper :: FileTreeC -> IO FileTreeC
deeper = cataA algA
  where
        algA
          :: FF.FreeF
               (Compose (Env [FilePath]) (M.Map FilePath))
               (Cofree (Compose IO (M.Map FilePath)) [FilePath])
               (IO FileTreeC)
             -> IO FileTreeC
        algA (FF.Pure (_ :< Compose ioMap)) = do
            mapNext <- ioMap
            pure $ Free (Compose $ env (M.keys mapNext) (Pure <$> mapNext))
        algA (FF.Free envMap) = Free <$> sequenceA envMap

