{-# LANGUAGE FlexibleContexts #-}

module Torosuke.Store where

import Control.Monad.Reader
import Data.Aeson (ToJSON, decode, encode)
import qualified Data.Text as T
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import Torosuke.Types
import qualified Prelude (show)

data DumpPath = DumpPath {dpDir :: FilePath, dpName :: FilePath}

getDumpPath' :: MonadReader Env m => Text -> m DumpPath
getDumpPath' tname = do
  env <- ask
  let pair = envPair env
      interval = envInterval env
   in pure $
        DumpPath
          ( "store" <> "/" <> pairToText pair
          )
          (intervalToText interval <> getTname <> ".json")
  where
    getTname = if T.null tname then "" else "_" <> toString tname

getKlinesDumpPath :: MonadReader Env m => m DumpPath
getKlinesDumpPath = getDumpPath' ""

getKlinesAnalysisDumpPath :: MonadReader Env m => m DumpPath
getKlinesAnalysisDumpPath = getDumpPath' "analysis"

getKlinesHistoAnalysisDumpPath :: MonadReader Env m => m DumpPath
getKlinesHistoAnalysisDumpPath = getDumpPath' "analysis_histo"

instance Show DumpPath where
  show dpath = dpDir dpath <> "/" <> dpName dpath

dumpData :: ToJSON a => DumpPath -> a -> ReaderT Env IO ()
dumpData dpath kls = do
  env <- ask
  let cacheDir = ".cache" <> "/" <> dpDir dpath
  let cachePath = cacheDir <> "/" <> dpName dpath
  liftIO $ createDirectoryIfMissing True cacheDir
  let content = decodeUtf8 $ encode kls
  writeFile cachePath content
  liftIO $ renameFile cachePath $ show dpath
  liftIO $ envLog env $ "Wrote " <> show dpath

loadKlines :: DumpPath -> IO (Maybe Klines)
loadKlines dpath = do
  createDirectoryIfMissing True $ dpDir dpath
  exists <- doesFileExist $ show dpath
  if exists
    then
      ( do
          content <- readFile $ show dpath
          pure $ decode $ encodeUtf8 content
      )
    else pure Nothing
