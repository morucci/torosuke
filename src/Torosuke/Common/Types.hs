module Torosuke.Common.Types where

import Data.Aeson
import Data.Time.Clock
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
