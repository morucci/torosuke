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

liveRunner :: ReaderT Env IO ()
liveRunner = do
  run
  where
    run = do
      _ <- pairFetcherAndAnalyzer Nothing 600 True
      _ <- wait
      run
    wait = do
      print $ "Waiting " <> delayStr delay <> "s for next iteration ..."
      liftIO $ threadDelay (1000000 * delay)
    delay :: Int
    delay = 10

historicalRunner :: UTCTime -> UTCTime -> ReaderT Env IO ()
historicalRunner startDate endDate = do
  _ <- run startDate
  pure ()
  where
    run :: UTCTime -> ReaderT Env IO ()
    run date = do
      lastCandleDate <- pairFetcherAndAnalyzer (Just date) 1000 False
      if lastCandleDate <= endDate
        then do
          print ("Reached request end date. Stopping." :: String)
          pure ()
        else do
          print $ "Waiting " <> delayStr delay <> "s for next iteration ..."
          liftIO $ threadDelay (1000000 * delay)
          run lastCandleDate
    delay :: Int
    delay = 1
