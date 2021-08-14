module Torosuke.Store where

import Data.Aeson (ToJSON, decode, encode)
import qualified Data.Text as T
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import Torosuke.Types
import qualified Prelude (show)

data DumpPath = DumpPath {dpDir :: FilePath, dpName :: FilePath}

getDumpPath' :: Text -> Pair -> Interval -> DumpPath
getDumpPath' tname pair interval =
  DumpPath
    ( "store" <> "/" <> pairToText pair
    )
    (intervalToText interval <> getTname <> ".json")
  where
    getTname = if T.null tname then "" else "_" <> toString tname

getKlinesDumpPath :: Pair -> Interval -> DumpPath
getKlinesDumpPath = getDumpPath' ""

getKlinesDumpPathLast100 :: Pair -> Interval -> DumpPath
getKlinesDumpPathLast100 = getDumpPath' "last_100"

getKlinesAnalysisDumpPath :: Pair -> Interval -> DumpPath
getKlinesAnalysisDumpPath = getDumpPath' "analysis"

instance Show DumpPath where
  show dpath = dpDir dpath <> "/" <> dpName dpath

dumpData :: ToJSON a => DumpPath -> a -> IO ()
dumpData dpath kls = do
  let cacheDir = ".cache" <> "/" <> dpDir dpath
  let cachePath = cacheDir <> "/" <> dpName dpath
  createDirectoryIfMissing True cacheDir
  let content = decodeUtf8 $ encode kls
  writeFile cachePath content
  renameFile cachePath $ show dpath

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
