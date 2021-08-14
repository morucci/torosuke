module Torosuke.Runner where

import qualified Data.HashMap.Strict as HM
import Relude
import Torosuke.Binance
import Torosuke.Store
import Torosuke.Ta
import Torosuke.Types

runner :: Pair -> Interval -> IO ()
runner pair interval = do
  let klinesDP = getKlinesDumpPath pair interval
      klinesDPLast100 = getKlinesDumpPathLast100 pair interval
      klinesAnalysis = getKlinesAnalysisDumpPath pair interval
  stored <- loadKlines klinesDP
  fetchedM <- getKlines pair interval 500 Nothing
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
