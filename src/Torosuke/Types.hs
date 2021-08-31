module Torosuke.Types where

import Control.Monad.Reader
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.HTTP.Types
import Relude
import Prelude (head)
import qualified Prelude (show)

-- | App env
newtype Pair = Pair {unPair :: String}

data Interval = ONE_H | ONE_D | HALF_H

data Env = Env
  { envPair :: Pair,
    envInterval :: Interval,
    envLog :: String -> IO ()
  }

pairToText :: Pair -> String
pairToText = unPair

textToInterval :: (IsString a, Eq a) => a -> Interval
textToInterval = \case
  "30m" -> HALF_H
  "30M" -> HALF_H
  "1H" -> ONE_H
  "1h" -> ONE_H
  "1D" -> ONE_D
  "1d" -> ONE_D
  _ -> error "Unsupported interval"

intervalToText :: Interval -> [Char]
intervalToText = \case
  ONE_D -> "1d"
  ONE_H -> "1h"
  HALF_H -> "30m"

-- | Generic Kline
data Kline = Kline
  { openT :: UTCTime,
    open :: Double,
    high :: Double,
    low :: Double,
    close :: Double,
    volume :: Double,
    closeT :: UTCTime
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Kline

instance ToJSON Kline

newtype Klines = Klines {kGet :: [Kline]} deriving (Show, Generic)

instance FromJSON Klines

instance ToJSON Klines

newtype KlinesHM = KlinesHM {unKlinesHM :: HM.HashMap Text Kline} deriving (Show, Generic)

getLast100Klines :: Klines -> Klines
getLast100Klines Klines {..} = Klines $ reverse $ take 100 $ reverse $ sort kGet

getLastKline :: [Kline] -> Kline
getLastKline = Prelude.head . sort

getLastDate :: Klines -> UTCTime
getLastDate Klines {..} = closeT $ getLastKline kGet

getLastClosePrice :: Klines -> Double
getLastClosePrice Klines {..} = close $ getLastKline kGet

getLastLowPrice :: Klines -> Double
getLastLowPrice Klines {..} = low $ getLastKline kGet

getLastHighPrice :: Klines -> Double
getLastHighPrice Klines {..} = high $ getLastKline kGet

mergeKlines :: Klines -> Klines -> Klines
mergeKlines set1 set2 =
  Klines $
    sort $
      HM.elems $
        HM.union (unKlinesHM $ toKlinesHM set1) (unKlinesHM $ toKlinesHM set2)

emptyKlinesHM :: KlinesHM
emptyKlinesHM = KlinesHM HM.empty

toKlinesHM :: Klines -> KlinesHM
toKlinesHM kls = KlinesHM $ HM.fromList $ toTuple <$> kGet kls
  where
    toTuple kl = (getK kl, kl)
    getK Kline {openT} = toText $ formatTime defaultTimeLocale "%s" openT

getCloseP :: Klines -> [Double]
getCloseP kls = map getCP $ kGet kls
  where
    getCP :: Kline -> Double
    getCP Kline {..} = close

getCloseT :: Klines -> [UTCTime]
getCloseT kls = map getCT $ kGet kls
  where
    getCT :: Kline -> UTCTime
    getCT Kline {..} = closeT

-- | Klines fetcher Response
data KlinesHTTPResponse = KlinesHTTPResponse
  { reStatus :: Status,
    reHeasders :: ResponseHeaders,
    reKlines :: Maybe Klines
  }

-- | TA data types and useful functions
data Macd = Macd {macdLine :: [Double], signalLine :: [Double]} deriving (Show, Generic)

instance ToJSON Macd

instance FromJSON Macd

data MacdAnalysis = MacdAnalysis
  { maCS :: [Bool],
    maSLAZ :: [Bool],
    maMLAZ :: [Bool],
    maMVASL :: [Bool]
  }
  deriving (Show, Generic)

instance ToJSON MacdAnalysis

instance FromJSON MacdAnalysis

data Analysis = Analysis
  { aKlines :: Klines,
    aMacd :: Macd,
    aMacdAnalisys :: MacdAnalysis,
    aDate :: [UTCTime],
    aCloseT :: UTCTime
  }
  deriving (Show, Generic)

cT :: Analysis -> UTCTime
cT Analysis {..} = aCloseT

addAnalysis :: Analysises -> Analysis -> Analysises
addAnalysis (Analysises store) toAdd = Analysises $ toAdd : store

instance ToJSON Analysis

instance FromJSON Analysis

newtype Analysises = Analysises
  { unAnalysises :: [Analysis]
  }
  deriving (Show, Generic)

instance FromJSON Analysises

instance ToJSON Analysises

instance Eq Analysis where
  (==) a b = cT a == cT b

instance Ord Analysis where
  compare a b = compare (cT a) (cT b)

sortAnalysises :: Analysises -> Analysises
sortAnalysises (Analysises anls) = Analysises $ sort anls

-- | Store datatype and functions
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
