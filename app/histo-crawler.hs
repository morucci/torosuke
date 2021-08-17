{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Reader
import Data.Time.Clock
import Options.Generic
import Relude
import Torosuke.Runner
import Torosuke.Types

data TorosukeHistoCli w = TorosukeHistoCli
  { pair :: w ::: String <?> "Pair name",
    interval :: w ::: Text <?> "Interval name",
    start :: w ::: Text <?> "Start date (format: '2020-01-01 00:00:00 Z'",
    end :: w ::: Text <?> "End date (format: '2020-01-01 00:00:00 Z'"
  }
  deriving stock (Generic)

instance ParseRecord (TorosukeHistoCli Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving stock instance Show (TorosukeHistoCli Unwrapped)

parseDate :: Text -> UTCTime
parseDate dateStr =
  fromMaybe
    (error "Wrong date format")
    (readMaybe $ toString dateStr :: Maybe UTCTime)

main :: IO ()
main = do
  args <- unwrapRecord "Torosuke histo crawler CLI"
  go args
  where
    go :: TorosukeHistoCli Unwrapped -> IO ()
    go args = do
      let appEnv = Env (Pair $ pair args) (textToInterval $ interval args)
      runReaderT
        ( historicalRunner
            (parseDate $ start args)
            (parseDate $ end args)
        )
        appEnv
