module Torosuke.Store where

import Control.Exception.Safe
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, renameFile)
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

loadData :: FromJSON a => DumpPath -> IO (Maybe a)
loadData dpath = do
  createDirectoryIfMissing True $ dpDir dpath
  exists <- doesFileExist $ show dpath
  if exists
    then
      ( do
          content <- readFile $ show dpath
          pure $ decode $ encodeUtf8 content
      )
    else pure Nothing

loadKlines :: DumpPath -> IO (Maybe Klines)
loadKlines = loadData

loadAnalysis :: DumpPath -> IO (Maybe Analysis)
loadAnalysis = loadData

loadPairAnalysis :: (MonadIO m, MonadReader Env m) => m (Maybe Analysis)
loadPairAnalysis = do
  dumpPath <- getKlinesAnalysisDumpPath
  liftIO $ loadAnalysis dumpPath

loadAllPairAnalysis :: (MonadIO m) => m [((String, String), Analysis)]
loadAllPairAnalysis = do
  entries <- liftIO $ listDirectory storePath
  let envs = concatMap getEnvs entries
  allAnalysis' <- traverse getAnalysis envs
  pure $ catMaybes allAnalysis'
  where
    getEnvs :: String -> [Env]
    getEnvs pairName =
      (\interval -> Env (Pair pairName) interval (const $ pure ()))
        <$> allInterval
    getAnalysis :: MonadIO m => Env -> m (Maybe ((String, String), Analysis))
    getAnalysis env = do
      anaM <- runReaderT loadPairAnalysis env
      pure $
        (\ana -> ((pairToText $ envPair env, intervalToText $ envInterval env), ana))
          <$> anaM
