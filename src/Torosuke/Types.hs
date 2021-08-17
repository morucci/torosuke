module Torosuke.Types where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.HTTP.Types
import Relude
import Prelude (head)

data Env = Env
  { envPair :: Pair,
    envInterval :: Interval
  }

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

instance FromJSON KlinesHM

instance ToJSON KlinesHM

newtype Pair = Pair {unPair :: String}

pairToText :: Pair -> String
pairToText = unPair

data Interval = ONE_H | ONE_D

textToInterval :: (IsString a, Eq a) => a -> Interval
textToInterval = \case
  "1H" -> ONE_H
  "1h" -> ONE_H
  "1D" -> ONE_D
  "1d" -> ONE_D
  _ -> error "Unsupported interval"

intervalToText :: Interval -> [Char]
intervalToText = \case
  ONE_D -> "1d"
  ONE_H -> "1h"

data KlinesHTTPResponse = KlinesHTTPResponse
  { reStatus :: Status,
    reHeasders :: ResponseHeaders,
    reKlines :: Maybe Klines
  }

getLast100Klines :: Klines -> Klines
getLast100Klines Klines {..} = Klines $ reverse $ take 100 $ reverse $ sort kGet

getLastDate :: Klines -> UTCTime
getLastDate Klines {..} = closeT $ Prelude.head $ sort kGet

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
