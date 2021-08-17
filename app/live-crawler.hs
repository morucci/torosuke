{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Reader
import Options.Generic
import Relude
import Torosuke.Runner
import Torosuke.Types

data TorosukeLiveCli w = TorosukeLiveCli
  { pair :: w ::: String <?> "Pair name",
    interval :: w ::: Text <?> "Interval name"
  }
  deriving stock (Generic)

instance ParseRecord (TorosukeLiveCli Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving stock instance Show (TorosukeLiveCli Unwrapped)

main :: IO ()
main = do
  args <- unwrapRecord "Torosuke live crawler CLI"
  go args
  where
    go :: TorosukeLiveCli Unwrapped -> IO ()
    go args = do
      let appEnv = Env (Pair $ pair args) (textToInterval $ interval args) toroLogger
      runReaderT liveRunner appEnv
