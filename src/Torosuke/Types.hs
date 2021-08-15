module Torosuke.Types where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.HTTP.Types
import Relude

data Kline = Kline
  { openT :: UTCTime,
    open :: Float,
    high :: Float,
    low :: Float,
    close :: Float,
    volume :: Float,
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

getK :: Kline -> Text
getK Kline {openT} = toText $ formatTime defaultTimeLocale "%s" openT

emptyKlinesHM :: KlinesHM
emptyKlinesHM = KlinesHM HM.empty

toKlinesHM :: Klines -> KlinesHM
toKlinesHM kls = KlinesHM $ HM.fromList $ toTuple <$> kGet kls
  where
    toTuple kl = (getK kl, kl)

getCloseP :: Klines -> [Float]
getCloseP kls = map getCP $ kGet kls
  where
    getCP :: Kline -> Float
    getCP Kline {..} = close

getCloseT :: Klines -> [UTCTime]
getCloseT kls = map getCT $ kGet kls
  where
    getCT :: Kline -> UTCTime
    getCT Kline {..} = closeT
