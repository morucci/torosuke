module Torosuke.Runner where

import qualified Data.HashMap.Strict as HM
import Data.Time.Clock
import Relude
import Torosuke.Binance
import Torosuke.Store
import Torosuke.Ta
import Torosuke.Types

pairFetcherAndAnalyzer :: Pair -> Interval -> Maybe UTCTime -> IO ()
pairFetcherAndAnalyzer pair interval until = do
  let klinesDP = getKlinesDumpPath pair interval
      klinesDPLast100 = getKlinesDumpPathLast100 pair interval
      klinesAnalysis = getKlinesAnalysisDumpPath pair interval
  stored <- loadKlines klinesDP
  resp <- getKlines pair interval 500 until
  let (KlinesHTTPResponse status _ fetchedM) = resp
  log pair interval status
  let (updatedKlines, analysis) = case (stored, fetchedM) of
        (Nothing, Nothing) -> error "Unable to decode dump and to fetch from API"
        (Just _, Nothing) -> error "Unable to fetch from API"
        (Nothing, Just fetched') -> (fetched', getTAAnalysis fetched')
        (Just stored', Just fetched') -> (merge stored' fetched', getTAAnalysis fetched')
  dumpData klinesDP updatedKlines
  dumpData klinesDPLast100 (Klines $ take 100 $ kGet updatedKlines)
  dumpData klinesAnalysis analysis
  where
    merge set1 set2 =
      Klines $
        HM.elems $
          HM.union (unKlinesHM $ toKlinesHM set1) (unKlinesHM $ toKlinesHM set2)
    log pair' interval' status =
      print $
        "Fetching 500 candles - pair: "
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

runner :: IO ()
runner = do
  _ <- pairFetcherAndAnalyzer ADAUSDT ONE_D Nothing
  _ <- pairFetcherAndAnalyzer ADAUSDT ONE_H Nothing
  pure ()
