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

data Env = Env
  { envPair :: Pair,
    envInterval :: Interval
  }

pairFetcherAndAnalyzer :: Maybe UTCTime -> Int -> Bool -> ReaderT Env IO UTCTime
pairFetcherAndAnalyzer until depth dumpAnalysis = do
  env <- ask
  let pair = envPair env
      interval = envInterval env
      klinesDP = getKlinesDumpPath pair interval
      klinesAnalysis = getKlinesAnalysisDumpPath pair interval
  stored <- liftIO $ loadKlines klinesDP
  resp <- liftIO $ getKlines pair interval depth until
  let (KlinesHTTPResponse status _ fetchedM) = resp
  log pair interval depth status
  let (updatedKlines, analysis, lastCandleDate) = case (stored, fetchedM) of
        (Nothing, Nothing) -> error "Unable to decode dump and to fetch from API"
        (Just _, Nothing) -> error "Unable to fetch from API"
        (Nothing, Just fetched') -> (fetched', getTAAnalysis fetched', getLastDate fetched')
        (Just stored', Just fetched') ->
          let merged = merge stored' fetched'
           in (merged, getTAAnalysis fetched', getLastDate fetched')
  liftIO $ dumpData klinesDP updatedKlines
  if dumpAnalysis then liftIO $ dumpData klinesAnalysis analysis else pure ()
  pure lastCandleDate
  where
    merge set1 set2 =
      Klines $
        sort $
          HM.elems $
            HM.union (unKlinesHM $ toKlinesHM set1) (unKlinesHM $ toKlinesHM set2)
    log pair' interval' depth' status =
      print $
        "Fetching "
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

liveRunner :: Pair -> Interval -> IO ()
liveRunner pair interval = do
  let runEnv = Env pair interval
  run runEnv
  where
    run runEnv' = do
      _ <- runReaderT (pairFetcherAndAnalyzer Nothing 600 True) runEnv'
      _ <- wait
      run runEnv'
    wait = do
      print $ "Waiting " <> delayStr delay <> "s for next iteration ..."
      threadDelay (1000000 * delay)
    delay :: Int
    delay = 10

historicalRunner :: Pair -> Interval -> UTCTime -> UTCTime -> IO ()
historicalRunner pair interval startDate endDate = do
  let runEnv = Env pair interval
  _ <- run startDate runEnv
  pure ()
  where
    run :: UTCTime -> Env -> IO ()
    run date runEnv' = do
      lastCandleDate <- runReaderT (pairFetcherAndAnalyzer (Just date) 1000 False) runEnv'
      if lastCandleDate <= endDate
        then do
          print ("Reached request end date. Stopping." :: String)
          pure ()
        else do
          print $ "Waiting " <> delayStr delay <> "s for next iteration ..."
          threadDelay (1000000 * delay)
          run lastCandleDate runEnv'
    delay :: Int
    delay = 1
