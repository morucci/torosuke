module Torosuke.Store where

import Control.Exception.Safe
import Data.Aeson (ToJSON, decode, encode)
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import Torosuke.Types

dumpData :: ToJSON a => DumpPath -> a -> ReaderT Env IO ()
dumpData dpath jdata = do
  env <- ask
  let cacheDir = ".cache" <> "/" <> dpDir dpath
  let cachePath = cacheDir <> "/" <> dpName dpath
  liftIO $ createDirectoryIfMissing True cacheDir
  let content = decodeUtf8 $ encode jdata
  current_content <- liftIO $ getDataFromFile $ show dpath
  when (current_content /= content) $ do
    writeFile cachePath content
    liftIO $ renameFile cachePath $ show dpath
    liftIO $ envLog env $ "Wrote " <> show dpath

getDataFromFile :: String -> IO String
getDataFromFile path = do
  result <- tryAny $ readFile path
  case result of
    Right content -> pure content
    Left _ -> pure ""

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
