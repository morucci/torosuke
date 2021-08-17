module Torosuke.Runner where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock
import Relude
import Torosuke.Binance
import Torosuke.Store
import Torosuke.Ta
import Torosuke.Types

toroLogger :: String -> IO ()
toroLogger = print

pairFetcherAndAnalyzer :: Maybe UTCTime -> Int -> ReaderT Env IO (UTCTime, Klines, Analysis)
pairFetcherAndAnalyzer until depth = do
  env <- ask

  let pair = envPair env
      interval = envInterval env
      logger = envLog env

  -- Read current candles from the store
  klinesDP <- getKlinesDumpPath
  stored <- liftIO $ loadKlines klinesDP
  -- Get klines from the API
  resp <- getKlines depth until

  let (KlinesHTTPResponse status _ fetchedM) = resp
      toLog = getLogLine pair interval depth status

  liftIO $ logger toLog

  -- Check status and merge candle and/or compute analysis according to status
  let (updatedKlines, analysis, lastCandleDate) = case (stored, fetchedM) of
        (Nothing, Nothing) -> error "Unable to decode dump and to fetch from API"
        (Just _, Nothing) -> error "Unable to fetch from API"
        (Nothing, Just fetched') -> (fetched', getTAAnalysis fetched', getLastDate fetched')
        (Just stored', Just fetched') ->
          let merged = mergeKlines stored' fetched'
           in (merged, getTAAnalysis fetched', getLastDate fetched')
  liftIO $ logger "Performed analysis of klines"

  -- Return last fetched candle
  pure (lastCandleDate, updatedKlines, analysis)
  where
    getLogLine pair' interval' depth' status =
      "Fetched "
        <> show depth'
        <> " candles - pair: "
        <> pairToText pair'
        <> ", interval: "
        <> intervalToText interval'
        <> ", until: "
        <> ( if isJust until
               then show until
               else "now"
           )
        <> " - status: "
        <> show status

delayStr :: Show a => a -> String
delayStr delay = toString (show delay :: String)

waitDelay :: Int -> ReaderT Env IO ()
waitDelay delay = do
  env <- ask
  liftIO $ envLog env $ "Waiting " <> delayStr delay <> "s for next iteration ..."
  liftIO $ threadDelay (1000000 * delay)

dumpDatas :: Klines -> Analysis -> Bool -> ReaderT Env IO ()
dumpDatas updatedKlines analysis dumpAnalysis = do
  env <- ask
  -- Get path to store fetched and computed data
  klinesDP <- getKlinesDumpPath
  klinesAnalysisDP <- getKlinesAnalysisDumpPath
  liftIO $ dumpData klinesDP updatedKlines
  if dumpAnalysis
    then liftIO $ dumpData klinesAnalysisDP analysis
    else pure ()
  liftIO $ envLog env "Dump logs on disk"

liveRunner :: ReaderT Env IO ()
liveRunner = do
  run
  where
    run = do
      (_, updatedKlines, analysis) <- pairFetcherAndAnalyzer Nothing 600
      _ <- dumpDatas updatedKlines analysis True
      _ <- waitDelay 10
      run

historicalRunner :: UTCTime -> UTCTime -> ReaderT Env IO ()
historicalRunner startDate endDate = do
  _ <- run startDate
  pure ()
  where
    run :: UTCTime -> ReaderT Env IO ()
    run date = do
      env <- ask
      (lastCandleDate, updatedKlines, analysis) <- pairFetcherAndAnalyzer (Just date) 1000
      _ <- dumpDatas updatedKlines analysis False
      if lastCandleDate <= endDate
        then liftIO $ envLog env ("Reached request end date. Stopping." :: String)
        else waitDelay 1
