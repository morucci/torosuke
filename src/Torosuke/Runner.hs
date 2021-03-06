module Torosuke.Runner where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad.Reader
import Data.Time
import Relude
import Torosuke.Binance
import Torosuke.Store
import Torosuke.Ta
import Torosuke.Types
import Prelude (head)

toroLogger :: String -> IO ()
toroLogger msg = do
  now <- getCurrentTime
  putStrLn $ "[" <> show now <> "] " <> msg

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
  let (updatedKlines, fetched) = case (stored, fetchedM) of
        (Nothing, Nothing) -> error "Unable to decode dump and to fetch from API"
        (Just _, Nothing) -> error "Unable to fetch from API"
        (Nothing, Just fetched') -> (fetched', fetched')
        (Just stored', Just fetched') ->
          let merged = mergeKlines fetched' stored'
           in (merged, fetched')
  liftIO $ logger "Performed analysis of klines"

  -- Return last fetched candle
  pure (getLastDate fetched, updatedKlines, getTAAnalysis fetched)
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
  -- Get path to store fetched and computed data
  klinesDP <- getKlinesDumpPath
  klinesAnalysisDP <- getKlinesAnalysisDumpPath
  klineCurrentDP <- getCurrentKlineDumpPath
  dumpData klinesDP updatedKlines
  dumpData klineCurrentDP $ Prelude.head $ reverse $ kGet updatedKlines
  if dumpAnalysis
    then dumpData klinesAnalysisDP analysis
    else pure ()

multiLiveRunner :: [(Pair, Interval)] -> IO ()
multiLiveRunner = mapConcurrently_ task
  where
    task :: (Pair, Interval) -> IO ()
    task tpl = runReaderT task' (toEnv tpl)
    toEnv (pair, interval) = Env pair interval toroLogger
    task' = do
      -- A depth of 100 seems to consume of weight of 1
      -- theoricaly we can perform 1200 requests by minutes
      (_, updatedKlines, analysis) <- pairFetcherAndAnalyzer Nothing 100
      void $ dumpDatas updatedKlines analysis True
      void $ waitDelay 10
      task'

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
        else do
          waitDelay 1
          run lastCandleDate

loadStoredKlines :: (MonadIO m, MonadReader Env m) => m Klines
loadStoredKlines = do
  klinesDP <- getKlinesDumpPath
  storedM <- liftIO $ loadKlines klinesDP
  case storedM of
    Just kls -> pure kls
    Nothing -> error $ "Unable to read klines from " <> show klinesDP

displayStoredKlines :: ReaderT Env IO ()
displayStoredKlines = do
  klines <- loadStoredKlines
  liftIO $ traverse_ printer $ kGet klines
  where
    printer :: Kline -> IO ()
    printer kline = print (show kline :: Text)

analysisOnStoredKlines :: ReaderT Env IO ()
analysisOnStoredKlines = do
  klines' <- loadStoredKlines
  let klines = kGet klines'
  liftIO $ print $ "Processing " <> show (length klines) <> (" candles ... (this may take a while)" :: Text)
  analysisStepDP <- getKlinesHistoAnalysisDumpPath
  analisys <- run (-1) klines $ Analysises []
  dumpData analysisStepDP analisys
  where
    run :: Int -> [Kline] -> Analysises -> ReaderT Env IO Analysises
    run offset klines acc = do
      let newOffset = offset + 1
          series = Klines $ slice klines (length klines - depth - offset) (length klines - offset)
      if newOffset <= length klines - depth
        then run newOffset klines $ addAnalysis acc $ getTAAnalysis series
        else pure acc
    slice l i k = drop i $ take k l
    depth = 100
