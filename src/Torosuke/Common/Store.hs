module Torosuke.Common.Store where

import Data.Aeson (decode, encode)
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import Torosuke.Common.Types
import qualified Prelude (show)

data DumpPath = DumpPath {dpDir :: FilePath, dpName :: FilePath}

getDumpPath :: Text -> Text -> Text -> DumpPath
getDumpPath provider pair interval =
  DumpPath
    ( toString $
        provider <> "/" <> pair
    )
    (toString $ interval <> ".json")

instance Show DumpPath where
  show dpath = dpDir dpath <> "/" <> dpName dpath

dumpKlines :: DumpPath -> KlinesHM -> IO ()
dumpKlines dpath kls = do
  let cacheDir = ".cache" <> "/" <> dpDir dpath
  let cachePath = cacheDir <> "/" <> dpName dpath
  createDirectoryIfMissing True cacheDir
  let content = decodeUtf8 $ encode kls
  writeFile cachePath content
  renameFile cachePath $ show dpath

loadKlines :: DumpPath -> IO (Maybe KlinesHM)
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
