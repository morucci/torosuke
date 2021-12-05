module Torosuke.Ui where

import Brick
import Brick.BChan
import Brick.Widgets.Table (renderTable, table)
import Control.Concurrent (forkIO, threadDelay)
import Data.Time.Format
import qualified Graphics.Vty as V
import Numeric
import Relude
import Torosuke.Store (loadAllPairAnalysis)
import Torosuke.Types
  ( Analysis (aCloseT, aKlines, aMacd, aMacdAnalisys),
    AnnotatedAnalysis (unAnnotatedAnalysis),
    Kline (close, volume),
    Klines,
    Macd (macdLine, signalLine),
    MacdAnalysis (maMVASL, maSLAZ),
    kGet,
  )
import Witch
import Prelude

newtype Tick = Tick [AnnotatedAnalysis]

data Name = Viewport1 deriving (Eq, Ord, Show)

data AppState = AppState
  { analysis :: [AnnotatedAnalysis],
    ticks :: Int
  }

showFullPrecision :: Int -> Double -> String
showFullPrecision p x = showFFloat (Just p) x ""

app :: App AppState Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

drawUI :: AppState -> [Widget Name]
drawUI s = tableUi
  where
    tableUi =
      [ viewport Viewport1 Vertical $
          renderTable $ table $ [headerRow] <> dataRows
      ]
      where
        headerRow :: [Widget Name]
        headerRow =
          [ str "Pair/Interval",
            str "Last candle date",
            str "MLASL (p-2, p-1, p)",
            str "SLAZ (p-2, p-1, p)",
            str "ML-SL Diff (p-2, p-1, p)",
            str "Volume (p-2, p-1, p)",
            str "Close Price (p-2, p-1, p)"
          ]
        dataRows :: [[Widget Name]]
        dataRows = analysisToRow <$> sort (analysis s)
        analysisToRow :: AnnotatedAnalysis -> [Widget Name]
        analysisToRow ana' =
          [ str $ fst (fst ana) <> "/" <> snd (fst ana),
            str $ formatTime defaultTimeLocale "%F %R" $ aCloseT $ snd ana,
            maMVASLToWidget . aMacdAnalisys $ snd ana,
            maSLAZToWidget . aMacdAnalisys $ snd ana,
            macdToWidget . aMacd $ snd ana,
            klineVolumeToWidget . aKlines $ snd ana,
            klinePriceToWidget . aKlines $ snd ana
          ]
          where
            ana = unAnnotatedAnalysis ana'
        maMVASLToWidget :: MacdAnalysis -> Widget Name
        maMVASLToWidget macdA =
          hBox $ intersperse (str " ") (boolWidget <$> reverse (take 3 (maMVASL macdA)))
        maSLAZToWidget :: MacdAnalysis -> Widget Name
        maSLAZToWidget macdA =
          hBox $ intersperse (str " ") (boolWidget <$> reverse (take 3 (maSLAZ macdA)))
        macdToWidget :: Macd -> Widget Name
        macdToWidget macd = dlToEnhancedWidget 8 diff
          where
            diff :: [Double]
            diff =
              uncurry (-)
                <$> zip
                  (reverse (take 4 (macdLine macd)))
                  (reverse (take 4 (signalLine macd)))
        klineVolumeToWidget :: Klines -> Widget Name
        klineVolumeToWidget klines = dlToEnhancedWidget 0 vol
          where
            vol :: [Double]
            vol = volume <$> reverse (take 4 (kGet klines))
        klinePriceToWidget :: Klines -> Widget Name
        klinePriceToWidget klines = dlToEnhancedWidget 6 closeP
          where
            closeP :: [Double]
            closeP = close <$> reverse (take 4 (kGet klines))

data EVal = EVal
  { eVal :: Double,
    ePercent :: Double,
    eHigher :: Bool,
    eValShowPrecision :: Int
  }

instance From EVal (Widget Name) where
  from EVal {..} =
    withAttr (if eHigher then greenAttr else redAttr) $
      hBox
        [ str $ showFullPrecision eValShowPrecision eVal,
          str $ "(" <> showFullPrecision 2 ePercent <> "%)"
        ]

dlToEnhanced :: Int -> [Double] -> [EVal]
dlToEnhanced showP dls = reverse . fst $ foldr reduce ([], 0) $ reverse dls
  where
    reduce :: Double -> ([EVal], Double) -> ([EVal], Double)
    reduce val (evals, prev) = (EVal val percent higher showP : evals, val)
      where
        percent = if prev == 0 then 0 else abs $ 100 - (prev / val * 100.0)
        higher = val >= prev

dlToEnhancedWidget :: Int -> [Double] -> Widget Name
dlToEnhancedWidget showP dls =
  hBox $ intersperse (str " ") $ from <$> Prelude.tail (dlToEnhanced showP dls)

boolWidget :: Bool -> Widget Name
boolWidget False = withAttr redAttr $ str "False"
boolWidget True = withAttr greenAttr $ str "True"

greenAttr :: AttrName
greenAttr = "greenAttr"

redAttr :: AttrName
redAttr = "redAttr"

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KDown [])) = do
  let vp = viewportScroll Viewport1
  vScrollBy vp 1
  continue s
handleEvent s (VtyEvent (V.EvKey V.KUp [])) = do
  let vp = viewportScroll Viewport1
  vScrollBy vp (-1)
  continue s
handleEvent s (AppEvent (Tick annotedAnalysis)) = do
  let nS =
        s
          { ticks = (s & ticks) + 1,
            analysis = annotedAnalysis
          }
  continue nS
handleEvent s _ = continue s

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (redAttr, fg V.red),
      (greenAttr, fg V.green)
    ]

getInitialState :: MonadIO m => m AppState
getInitialState = do
  analysis <- loadAllPairAnalysis
  pure $ AppState analysis 0

main :: IO ()
main = do
  chan <- newBChan 10
  initialState <- getInitialState
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void
    . forkIO
    . forever
    $ do
      analysis <- loadAllPairAnalysis
      writeBChan chan $ Tick analysis
      threadDelay 100000
  void $ customMain initialVty buildVty (Just chan) app initialState
