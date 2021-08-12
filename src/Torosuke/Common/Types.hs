module Torosuke.Common.Types where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime)
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

getK :: Kline -> Text
getK Kline {openT} = toText $ formatTime defaultTimeLocale "%s" openT

emptyKlinesHM :: KlinesHM
emptyKlinesHM = KlinesHM HM.empty

toKlinesHM :: Klines -> KlinesHM
toKlinesHM kls = KlinesHM $ HM.fromList $ toTuple <$> kGet kls
  where
    toTuple kl = (getK kl, kl)
