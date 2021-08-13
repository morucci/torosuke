module Torosuke.Ta where

import Data.List ((!!))
import Data.Time.Clock
import Relude
import Torosuke.Common.Types

data Macd = Macd {macdLine :: [Float], signalLine :: [Float]} deriving (Show)

data Analysis = Analysis
  { aKlines :: Klines,
    aMacd :: Macd,
    aMacdCL :: [Bool],
    aDate :: [UTCTime]
  }
  deriving (Show)

_ema :: Int -> [Float] -> Float
_ema period series = case series of
  [] -> 0
  [x] -> x
  x : xs -> (alpha * x) + ((1 - alpha) * _ema period xs)
  where
    alpha :: Float
    alpha = (2.0 :: Float) / fromRational (toRational period + 1)

ema :: Int -> [Float] -> [Float]
ema period series =
  take (length series) $ run 0 $ reverse series
  where
    run i series' =
      if i == length series
        then series'
        else _ema period (slice series' i (length series)) : run (i + 1) series'
    slice l i k = drop i $ take k l

macd_12_26_9 :: [Float] -> Macd
macd_12_26_9 series =
  let ema_12 = ema 12 series
      ema_26 = ema 26 series
      macd_line = compute_macd_line ema_12 ema_26
      signal_line = ema 9 $ reverse macd_line
   in Macd macd_line signal_line
  where
    compute_macd_line :: [Float] -> [Float] -> [Float]
    compute_macd_line s l =
      map (\(i, sv) -> sv - (!!) l i) $ zip [0 .. length s] s

crossSignal :: Macd -> [Bool]
crossSignal Macd {..} =
  let merged = zip macdLine signalLine
   in zipWith
        (curry check)
        (map computeMacdAboveSignal merged)
        (map computeMacdAboveSignal $ drop 1 merged)
  where
    computeMacdAboveSignal :: (Float, Float) -> Bool
    computeMacdAboveSignal (ml, sl) = ml > sl
    check (b1, b2) = not b1 == b2

getTAAnalysis :: Klines -> Analysis
getTAAnalysis kls =
  let closePrice = getCloseP kls
      aKlines = kls
      aMacd = macd_12_26_9 closePrice
      aMacdCL = crossSignal aMacd
      aDate = reverse $ getCloseT kls
   in Analysis {..}
