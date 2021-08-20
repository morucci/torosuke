module Torosuke.Store where

import Data.Aeson (ToJSON, decode, encode)
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import Torosuke.Types

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
