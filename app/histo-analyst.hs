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

data TorosukeHistoCli w = TorosukeHistoCli
  { pair :: w ::: String <?> "Pair name",
    interval :: w ::: Text <?> "Interval name"
  }
  deriving stock (Generic)

instance ParseRecord (TorosukeHistoCli Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving stock instance Show (TorosukeHistoCli Unwrapped)

main :: IO ()
main = do
  args <- unwrapRecord "Torosuke histo analyst CLI"
  go args
  where
    go :: TorosukeHistoCli Unwrapped -> IO ()
    go args = do
      let appEnv = Env (Pair $ pair args) (textToInterval $ interval args) toroLogger
      runReaderT analysisOnStoredKlines appEnv
