module Torosuke.Ta where

import Data.List ((!!))
import Relude
import Torosuke.Types
import Prelude (head)

_ema :: Int -> [Double] -> Double
_ema period series = case series of
  [] -> 0
  [x] -> x
  x : xs -> (alpha * x) + ((1 - alpha) * _ema period xs)
  where
    alpha :: Double
    alpha = (2.0 :: Double) / fromRational (toRational period + 1)

ema :: Int -> [Double] -> [Double]
ema period series =
  take (length series) $ run 0 $ reverse series
  where
    run i series' =
      if i == length series
        then series'
        else _ema period (slice series' i (length series)) : run (i + 1) series'
    slice l i k = drop i $ take k l

macd_12_26_9 :: [Double] -> Macd
macd_12_26_9 series =
  let ema_12 = ema 12 series
      ema_26 = ema 26 series
      macd_line = compute_macd_line ema_12 ema_26
      signal_line = ema 9 $ reverse macd_line
   in Macd macd_line signal_line
  where
    compute_macd_line :: [Double] -> [Double] -> [Double]
    compute_macd_line s l =
      zipWith (\i sv -> sv - (!!) l i) [0 .. length s] s

macdLineAboveSignal :: Macd -> [Bool]
macdLineAboveSignal Macd {..} =
  let merged = zip macdLine signalLine
   in map (uncurry (>)) merged

macdLineAboveZero :: Macd -> [Bool]
macdLineAboveZero Macd {..} = map (> 0) macdLine

signalLineAboveZero :: Macd -> [Bool]
signalLineAboveZero Macd {..} = map (> 0) signalLine

crossSignal :: Macd -> [Bool]
crossSignal Macd {..} =
  let merged = zip macdLine signalLine
   in zipWith
        check
        (map computeMacdAboveSignal merged)
        (map computeMacdAboveSignal $ drop 1 merged)
  where
    computeMacdAboveSignal (ml, sl) = ml > sl
    check b1 b2 = not b1 == b2

getTAAnalysis :: Klines -> Analysis
getTAAnalysis kls' =
  let kls = dropCurrent kls'
      closePrice = getCloseP kls
      aKlines = Klines $ reverse $ take depth $ reverse $ kGet kls
      aMacd =
        let macd' = macd_12_26_9 closePrice
         in -- Limit to 10 values in the report
            Macd (take depth $ macdLine macd') (take depth $ signalLine macd')
      aMacdAnalisys =
        MacdAnalysis
          (crossSignal aMacd)
          (signalLineAboveZero aMacd)
          (macdLineAboveZero aMacd)
          (macdLineAboveSignal aMacd)
      aDate = take depth $ reverse $ getCloseT kls
      aCloseT = Prelude.head aDate
   in Analysis {..}
  where
    depth = 5
