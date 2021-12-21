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
newtype Pair = Pair {unPair :: String} deriving (Eq, Ord)

data Interval
  = ONE_D
  | ONE_H
  | HALF_H
  | FIVETEEN_M
  | FIVE_M
  deriving (Enum, Bounded, Show, Eq, Ord)

data Env = Env
  { envPair :: Pair,
    envInterval :: Interval,
    envLog :: String -> IO ()
  }

pairToText :: Pair -> String
pairToText = unPair

textToInterval :: (IsString a, Eq a) => a -> Interval
textToInterval = \case
  "5m" -> FIVE_M
  "15m" -> FIVETEEN_M
  "30m" -> HALF_H
  "1h" -> ONE_H
  "1d" -> ONE_D
  _ -> error "Unsupported interval"

intervalToText :: Interval -> String
intervalToText = \case
  FIVE_M -> "5m"
  FIVETEEN_M -> "15m"
  ONE_D -> "1d"
  ONE_H -> "1h"
  HALF_H -> "30m"

allInterval :: [Interval]
allInterval = [minBound ..]

allInterval' :: [String]
allInterval' = map intervalToText allInterval

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

getLastKline :: [Kline] -> Kline
getLastKline = Prelude.head . sort

getLastDate :: Klines -> UTCTime
getLastDate Klines {..} = closeT $ getLastKline kGet

dropCurrent :: Klines -> Klines
dropCurrent Klines {..} = Klines $ reverse $ drop 1 $ reverse kGet

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

newtype AnnotatedAnalysis = AnnotatedAnalysis
  { unAnnotatedAnalysis :: ((Pair, Interval), Analysis)
  }

getAnnotations :: AnnotatedAnalysis -> (Pair, Interval)
getAnnotations a = fst $ unAnnotatedAnalysis a

instance Eq AnnotatedAnalysis where
  (==) a b = fst (unAnnotatedAnalysis a) == fst (unAnnotatedAnalysis b)

instance Ord AnnotatedAnalysis where
  (<=) a b = fst (unAnnotatedAnalysis a) <= fst (unAnnotatedAnalysis b)

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

storePath :: String
storePath = "store"

getDumpPath :: MonadReader Env m => Text -> m DumpPath
getDumpPath tname = do
  env <- ask
  let pair = envPair env
      interval = envInterval env
   in pure $
        DumpPath
          ( storePath <> "/" <> pairToText pair
          )
          (intervalToText interval <> getTname <> ".json")
  where
    getTname = if T.null tname then "" else "_" <> toString tname

getKlinesDumpPath :: MonadReader Env m => m DumpPath
getKlinesDumpPath = getDumpPath ""

getCurrentKlineDumpPath :: MonadReader Env m => m DumpPath
getCurrentKlineDumpPath = getDumpPath "current"

getKlinesAnalysisDumpPath :: MonadReader Env m => m DumpPath
getKlinesAnalysisDumpPath = getDumpPath "analysis"

getKlinesHistoAnalysisDumpPath :: MonadReader Env m => m DumpPath
getKlinesHistoAnalysisDumpPath = getDumpPath "analysis_histo"

instance Show DumpPath where
  show dpath = dpDir dpath <> "/" <> dpName dpath
