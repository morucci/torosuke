module Torosuke.Runner where

import Control.Concurrent (threadDelay)
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock
import Relude
import Torosuke.Binance
import Torosuke.Store
import Torosuke.Ta
import Torosuke.Types
import Prelude (head)

pairFetcherAndAnalyzer :: Pair -> Interval -> Maybe UTCTime -> Int -> IO UTCTime
pairFetcherAndAnalyzer pair interval until depth = do
  let klinesDP = getKlinesDumpPath pair interval
      klinesDPLast100 = getKlinesDumpPathLast100 pair interval
      klinesAnalysis = getKlinesAnalysisDumpPath pair interval
  stored <- loadKlines klinesDP
  resp <- getKlines pair interval depth until
  let (KlinesHTTPResponse status _ fetchedM) = resp
  log pair interval depth status
  let (updatedKlines, analysis) = case (stored, fetchedM) of
        (Nothing, Nothing) -> error "Unable to decode dump and to fetch from API"
        (Just _, Nothing) -> error "Unable to fetch from API"
        (Nothing, Just fetched') -> (fetched', getTAAnalysis fetched')
        (Just stored', Just fetched') -> (merge stored' fetched', getTAAnalysis fetched')
  dumpData klinesDP updatedKlines
  dumpData klinesDPLast100 (Klines $ take 100 $ kGet updatedKlines)
  dumpData klinesAnalysis analysis
  pure $ closeT $ Prelude.head $ kGet $ aKlines analysis
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
liveRunner pair interval = do run
  where
    run = do
      _ <- pairFetcherAndAnalyzer pair interval Nothing 100
      _ <- wait
      run
    wait = do
      print $ "Waiting " <> delayStr delay <> "s for next iteration ..."
      threadDelay (1000000 * delay)
    delay :: Int
    delay = 10

historicalRunner :: Pair -> Interval -> UTCTime -> UTCTime -> IO ()
historicalRunner pair interval startDate endDate = do
  _ <- run startDate
  pure ()
  where
    run :: UTCTime -> IO ()
    run date = do
      lastCandleDate <- pairFetcherAndAnalyzer pair interval (Just date) 100
      if lastCandleDate <= endDate
        then do
          print ("Reached request end date. Stopping." :: String)
          pure ()
        else do
          print $ "Waiting " <> delayStr delay <> "s for next iteration ..."
          threadDelay (1000000 * delay)
          run lastCandleDate
    delay :: Int
    delay = 10
